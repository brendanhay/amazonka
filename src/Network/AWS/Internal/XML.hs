{-# LANGUAGE DefaultSignatures               #-}
{-# LANGUAGE DeriveGeneric                   #-}
{-# LANGUAGE FlexibleContexts                #-}
{-# LANGUAGE FlexibleInstances               #-}
{-# LANGUAGE MultiParamTypeClasses           #-}
{-# LANGUAGE NoMonomorphismRestriction       #-}
{-# LANGUAGE OverlappingInstances            #-}
{-# LANGUAGE OverloadedStrings               #-}
{-# LANGUAGE Rank2Types                      #-}
{-# LANGUAGE ScopedTypeVariables             #-}
{-# LANGUAGE TypeOperators                   #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- Module      : Network.AWS.Internal.XML
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Internal.XML where

import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Char             (isLower, isSpace)
import           Data.Either
import           Data.Maybe
import           Data.Monoid
import           Data.String
import           Data.Text             (Text)
import qualified Data.Text             as Text
import qualified Data.Text.Encoding    as Text
import           Data.XML.Pickle
import           Data.XML.Types
import           GHC.Generics

data Options = Options
    { xmlCtorMod  :: String -> Name
    , xmlFieldMod :: String -> Name
    , xmlRootElem :: Maybe Name
    , xmlListElem :: Maybe Name
    }

defaultOptions :: Options
defaultOptions = Options fromString fromString Nothing Nothing

class IsXML a where
    xmlPickler :: PU [Node] a

    default xmlPickler :: (Generic a, GIsXML (Rep a)) => PU [Node] a
    xmlPickler = genericPickler defaultOptions

genericPickler opts = xpWrap to from $ (gPickler opts) (genericPickler opts)

toXML :: IsXML a => a -> Text



fromXML :: IsXML a => Text -> Either String a


class GIsXML f where
    gPickler :: Options -> PU [Node] a -> PU [Node] (f a)

instance IsXML a => GIsXML (K1 i a) where
    gPickler _ _ = xpWrap K1 unK1 xmlPickler

instance (Datatype d, GIsXML a) => GIsXML (D1 d a) where
    gPickler opts = xpWrap M1 unM1 . gPickler opts

instance (Constructor c, GIsXML a) => GIsXML (C1 c a) where
    gPickler opts = xpWrap M1 unM1 . gPickler opts

instance (GIsXML a, GIsXML b) => GIsXML (a :+: b) where
    gPickler opts f = gPickler opts f `xpSum` gPickler opts f

instance (GIsXML a, GIsXML b) => GIsXML (a :*: b) where
    gPickler opts f = xpWrap (uncurry (:*:)) (\(a :*: b) -> (a, b))
        (gPickler opts f <#> gPickler opts f)

instance (Selector s, GIsXML a) => GIsXML (S1 s a) where
    gPickler opts f = xpElemNodes
        (xmlFieldMod opts $ selName (undefined :: S1 s a p))
        (xpWrap M1 unM1 $ gPickler opts f)

instance (Selector s, IsXML a) => GIsXML (S1 s (K1 i (Maybe a))) where
    gPickler opts _ = xpWrap (M1 . K1) (unK1 . unM1) $
        xpOption (xpElemNodes name xmlPickler)
      where
        name = xmlFieldMod opts $ selName (undefined :: t s (K1 i (Maybe a)) p)

instance (Selector s, IsXML a) => GIsXML (S1 s (K1 i [a])) where
    gPickler opts _ =  xpWrap (M1 . K1)  (unK1 . unM1) . xpSeqWhile $
        xpElemNodes (fromMaybe name $ xmlListElem opts) xmlPickler
      where
        name = xmlFieldMod opts $ selName (undefined :: t s (K1 i [a]) p)

xpSum :: PU [t] (f r) -> PU [t] (g r) -> PU [t] ((f :+: g) r)
xpSum l r = xpWrap i o $ xpEither l r
  where
    i (Left  x) = L1 x
    i (Right x) = R1 x

    o (L1 x) = Left x
    o (R1 x) = Right x
