{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Gen.V2.JSON
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.V2.JSON where

import           Data.ByteString.Lazy    (ByteString)
import           Data.Function           (on)
import qualified Data.Jason.Encode       as Jason
import           Data.Jason.Types
import           Data.List               (intersperse, sortBy, elemIndex)
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.SemVer             (Version, fromText, toText)
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Data.Text.Lazy.Builder  (Builder, toLazyText)
import qualified Data.Text.Lazy.Encoding as Text
import qualified Data.Vector             as Vector

instance FromJSON Version where
    parseJSON = withText "semantic_version" $
        either fail return . fromText

instance ToJSON Version where
    toJSON = String . toText

data PState = PState
    { pstIndent :: Int
    , pstLevel  :: Int
    , pstSort   :: [(Text, Value)] -> [(Text, Value)]
    }

encodePretty :: ToJSON a => a -> ByteString
encodePretty = Text.encodeUtf8
    . toLazyText
    . fromValue (PState 4 0 sort') . toJSON
  where
    sort' = sortBy (mempty `on` fst)

fromValue :: PState -> Value -> Builder
fromValue st@PState{..} = go
  where
    go (Array  v) = fromCompound st ("[", "]") fromValue (Vector.toList v)
    go (Object o) = fromCompound st ("{", "}") fromPair (pstSort (unObject o))
    go v          = Jason.fromValue v

fromCompound :: PState
             -> (Builder, Builder)
             -> (PState -> a -> Builder)
             -> [a]
             -> Builder
fromCompound st@PState{..} (delimL, delimR) fromItem items = mconcat
    [ delimL
    , if null items
          then mempty
          else "\n" <> items' <> "\n" <> fromIndent st
    , delimR
    ]
  where
    items' = mconcat . intersperse ",\n" $
                map (\item -> fromIndent st' <> fromItem st' item)
                    items
    st' = st { pstLevel = pstLevel + 1 }

fromPair :: PState -> (Text, Value) -> Builder
fromPair st (k, v) = Jason.fromValue (toJSON k) <> ": " <> fromValue st v

fromIndent :: PState -> Builder
fromIndent PState{..} = mconcat $ replicate (pstIndent * pstLevel) " "
