{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.SimpleDB.V2009_04_15.GetAttributes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns all of the attributes associated with the specified item.
-- Optionally, the attributes returned can be limited to one or more
-- attributes by specifying an attribute name parameter. If the item does not
-- exist on the replica that was accessed for this operation, an empty set is
-- returned. The system does not return an error as it cannot guarantee the
-- item does not exist on other replicas. If GetAttributes is called without
-- being passed any attribute names, all the attributes for the item are
-- returned.
module Network.AWS.SimpleDB.V2009_04_15.GetAttributes where

import Control.Lens
import Network.AWS.Request.Query
import Network.AWS.SimpleDB.V2009_04_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'GetAttributes' request.
getAttributes :: Text -- ^ '_garItemName'
              -> Text -- ^ '_garDomainName'
              -> GetAttributes
getAttributes p1 p2 = GetAttributes
    { _garItemName = p1
    , _garDomainName = p2
    , _garAttributeNames = mempty
    , _garConsistentRead = Nothing
    }

data GetAttributes = GetAttributes
    { _garItemName :: Text
      -- ^ The name of the item.
    , _garDomainName :: Text
      -- ^ The name of the domain in which to perform the operation.
    , _garAttributeNames :: [Text]
      -- ^ The names of the attributes.
    , _garConsistentRead :: Maybe Bool
      -- ^ Determines whether or not strong consistency should be enforced
      -- when data is read from SimpleDB. If true, any data previously
      -- written to SimpleDB will be returned. Otherwise, results will be
      -- consistent eventually, and the client may not see data that was
      -- written immediately before your read.
    } deriving (Generic)

makeLenses ''GetAttributes

instance ToQuery GetAttributes where
    toQuery = genericToQuery def

data GetAttributesResponse = GetAttributesResponse
    { _gasAttributes :: [Attribute]
      -- ^ The list of attributes returned by the operation.
    } deriving (Generic)

makeLenses ''GetAttributesResponse

instance FromXML GetAttributesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetAttributes where
    type Sv GetAttributes = SimpleDB
    type Rs GetAttributes = GetAttributesResponse

    request = post "GetAttributes"
    response _ = xmlResponse
