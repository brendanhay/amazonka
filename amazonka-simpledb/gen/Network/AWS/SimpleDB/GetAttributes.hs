{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SimpleDB.GetAttributes
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
module Network.AWS.SimpleDB.GetAttributes
    (
    -- * Request
      GetAttributes
    -- ** Request constructor
    , getAttributes
    -- ** Request lenses
    , gaDomainName
    , gaItemName
    , gaAttributeName
    , gaConsistentRead

    -- * Response
    , GetAttributesResponse
    -- ** Response constructor
    , getAttributesResponse
    -- ** Response lenses
    , garAttribute
    ) where

import Network.AWS.Request.Query
import Network.AWS.SimpleDB.Types
import Network.AWS.Prelude

data GetAttributes = GetAttributes
    { _gaDomainName :: Text
    , _gaItemName :: Text
    , _gaAttributeName :: [Text]
    , _gaConsistentRead :: Maybe Bool
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetAttributes' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DomainName ::@ @Text@
--
-- * @ItemName ::@ @Text@
--
-- * @AttributeName ::@ @[Text]@
--
-- * @ConsistentRead ::@ @Maybe Bool@
--
getAttributes :: Text -- ^ 'gaDomainName'
              -> Text -- ^ 'gaItemName'
              -> GetAttributes
getAttributes p1 p2 = GetAttributes
    { _gaDomainName = p1
    , _gaItemName = p2
    , _gaAttributeName = mempty
    , _gaConsistentRead = Nothing
    }

-- | The name of the domain in which to perform the operation.
gaDomainName :: Lens' GetAttributes Text
gaDomainName = lens _gaDomainName (\s a -> s { _gaDomainName = a })

-- | The name of the item.
gaItemName :: Lens' GetAttributes Text
gaItemName = lens _gaItemName (\s a -> s { _gaItemName = a })

-- | The names of the attributes.
gaAttributeName :: Lens' GetAttributes [Text]
gaAttributeName = lens _gaAttributeName (\s a -> s { _gaAttributeName = a })

-- | Determines whether or not strong consistency should be enforced when data
-- is read from SimpleDB. If true, any data previously written to SimpleDB
-- will be returned. Otherwise, results will be consistent eventually, and the
-- client may not see data that was written immediately before your read.
gaConsistentRead :: Lens' GetAttributes (Maybe Bool)
gaConsistentRead =
    lens _gaConsistentRead (\s a -> s { _gaConsistentRead = a })

instance ToQuery GetAttributes where
    toQuery = genericQuery def

newtype GetAttributesResponse = GetAttributesResponse
    { _garAttribute :: [Attribute]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetAttributesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Attribute ::@ @[Attribute]@
--
getAttributesResponse :: GetAttributesResponse
getAttributesResponse = GetAttributesResponse
    { _garAttribute = mempty
    }

-- | The list of attributes returned by the operation.
garAttribute :: Lens' GetAttributesResponse [Attribute]
garAttribute = lens _garAttribute (\s a -> s { _garAttribute = a })

instance FromXML GetAttributesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetAttributes where
    type Sv GetAttributes = SimpleDB
    type Rs GetAttributes = GetAttributesResponse

    request = post "GetAttributes"
    response _ = xmlResponse
