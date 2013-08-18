{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
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

import Data.Time
import GHC.Generics
import Network.HTTP.QueryString.Pickle
import Text.XML.Expat.Pickle.Generic

newtype Member a = Member { member :: [a] }
    deriving (Eq, Show, Generic)

instance IsQuery a => IsQuery (Member a) where
    queryPickler = (Member, member) `qpWrap` qpOrdinalList queryPickler

instance IsQuery Bool where
    queryPickler = qpPrim

instance IsQuery () where
    queryPickler = qpLift ()

instance IsXML Bool where
    xmlPickler = xpPrim

instance IsXML UTCTime where
    xmlPickler = xpPrim
