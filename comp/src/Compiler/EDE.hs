{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}

-- Module      : Compiler.EDE
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.EDE where

import           Compiler.Types
import           Control.Error
import           Control.Lens
import           Data.Aeson     hiding (encode)
import           Data.Bifunctor
import qualified Data.Text.Lazy as LText
import           Text.EDE

-- makeLenses ''Template

-- typesTemplate, operationTemplate :: Getter (Templates a) (a -> Template)
-- typesTemplate     = to (\s -> fst . _selectTemplate s)
-- operationTemplate = to (\s -> snd . _selectTemplate s)

render :: Monad m => Template -> Value -> Compiler m LazyText
render x v = hoistEither
    . first LText.pack
    $ note ("Error serialising params: " ++ show v) (fromValue v)
  >>= eitherRender x
        --
--    filters = mempty -- Map.fromList
--        [ "indent" @: flip indent
        -- , "highlight"    @: highlightType
        -- , "parens"       @: parens
        -- , "wrapped"      @: wrapped
        -- , "concat"       @: (mappend :: Text -> Text -> Text)
        -- , "joinedLength" @: joinedLength
        -- , "member"       @: (elem :: Text -> [Text] -> Bool)
        -- , "waiter"       @: waiter
--        ]

-- parens :: Text -> Text
-- parens t = "(" <> t <> ")"

-- wrapped :: Text -> Text
-- wrapped t
--     | Text.null t        = t
--     | Text.head t == '[' = t
--     | otherwise          = maybe t (const (parens t)) (Text.findIndex isSpace t)

-- joinedLength :: [Text] -> Text -> Int
-- joinedLength xs sep = sum (map ((+n) . Text.length) xs)
--   where
--     n = Text.length sep

-- waiter :: Text -> Text
-- waiter t
--     | p "DB"    = "db" <> Text.drop 2 t
--     | otherwise = toCamel t
--   where
--     p = flip Text.isPrefixOf t
