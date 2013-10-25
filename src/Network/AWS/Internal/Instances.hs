{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}

{-# OPTIONS_GHC -fno-warn-orphans       #-}

-- Module      : Network.AWS.Internal.Instances
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Internal.Instances where

import           Control.Monad
import           Data.ByteString                 (ByteString)
import qualified Data.ByteString.Char8           as BS
import           Data.Foldable                   (Foldable)
import           Data.Time
import           GHC.Generics
import           Network.AWS.Internal.Time
import           Network.HTTP.QueryString.Pickle
import           Text.XML.Expat.Pickle.Generic

newtype Items a = Items { items :: [a] }
    deriving (Eq, Show, Generic, Foldable)

newtype Members a = Members { members :: [a] }
    deriving (Eq, Show, Generic, Foldable)

instance IsQuery a => IsQuery [a] where
    queryPickler = qpOrdinalList queryPickler

instance IsQuery a => IsQuery (Items a) where
    queryPickler = (Items, items) `qpWrap` qpOrdinalList queryPickler

instance IsXML a => IsXML (Items a) where
    xmlPickler = xpWrap (Items, items) $ xpElemList (name "item") pu
      where
        name = maybe mkAnNName mkNName . join $ nnNamespace `fmap` root pu
        pu   = xmlPickler

instance IsQuery a => IsQuery (Members a) where
    queryPickler = qpWrap (Members, members)
        (qpElem "member" $ qpOrdinalList queryPickler)

instance IsXML a => IsXML (Members a) where
    xmlPickler = xpWrap (Members, members) $ xpElemList (name "member") pu
      where
        name = maybe mkAnNName mkNName . join $ nnNamespace `fmap` root pu
        pu   = xmlPickler

instance IsQuery () where
    queryPickler = qpLift ()

instance IsQuery Bool where
    queryPickler = QueryPU (Value . lowerBool) u
      where
        u (Value s) = parseBool s
        u e         = Left $ "unable to encode Bool from: " ++ show e

instance IsXML Bool where
    xmlPickler = xpContent $ XMLPU lowerBool parseBool Nothing

instance IsQuery UTCTime where
    queryPickler = QueryPU (Value . formatISO8601) u
      where
        u (Value s) = parseISO8601 $ BS.unpack s
        u o         = Left $ "unable to parse ISO8601 time from: " ++ show o

instance IsXML UTCTime where
    xmlPickler = xpContent XMLPU
        { pickleTree   = formatISO8601
        , unpickleTree = parseISO8601 . BS.unpack
        , root         = Nothing
        }

instance IsQuery Double where
    queryPickler = qpPrim

lowerBool :: Bool -> ByteString
lowerBool True  = "true"
lowerBool False = "false"

parseBool :: ByteString -> Either String Bool
parseBool "true"  = Right True
parseBool "false" = Right False
parseBool e       = Left $ "unable to parse Bool from: " ++ show e
