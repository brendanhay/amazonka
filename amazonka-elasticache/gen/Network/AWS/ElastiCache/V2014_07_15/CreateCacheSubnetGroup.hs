{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.V2014_07_15.CreateCacheSubnetGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The CreateCacheSubnetGroup operation creates a new cache subnet group. Use
-- this parameter only when you are creating a cluster in an Amazon Virtual
-- Private Cloud (VPC). https://elasticache.amazonaws.com/
-- ?Action=CreateCacheSubnetGroup &CacheSubnetGroupName=myCachesubnetgroup
-- &CacheSubnetGroupDescription=My%20new%20CacheSubnetGroup
-- &Version=2014-03-24 &SignatureVersion=4 &SignatureMethod=HmacSHA256
-- &Timestamp=20140401T192317Z &X-Amz-Credential= 990524496922 My new
-- CacheSubnetGroup myCachesubnetgroup Active subnet-7c5b4115 us-east-1c
-- Active subnet-7b5b4112 us-east-1b Active subnet-3ea6bd57 us-east-1d
-- ed662948-a57b-11df-9e38-7ffab86c801f.
module Network.AWS.ElastiCache.V2014_07_15.CreateCacheSubnetGroup
    (
    -- * Request
      CreateCacheSubnetGroup
    -- ** Request constructor
    , mkCreateCacheSubnetGroup
    -- ** Request lenses
    , ccsg1CacheSubnetGroupName
    , ccsg1CacheSubnetGroupDescription
    , ccsg1SubnetIds

    -- * Response
    , CreateCacheSubnetGroupResponse
    -- ** Response constructor
    , mkCreateCacheSubnetGroupResponse
    -- ** Response lenses
    , ccsgrrCacheSubnetGroup
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_07_15.Types
import Network.AWS.Prelude

-- | Represents the input of a CreateCacheSubnetGroup operation.
data CreateCacheSubnetGroup = CreateCacheSubnetGroup
    { _ccsg1CacheSubnetGroupName :: Text
    , _ccsg1CacheSubnetGroupDescription :: Text
    , _ccsg1SubnetIds :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateCacheSubnetGroup' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CacheSubnetGroupName ::@ @Text@
--
-- * @CacheSubnetGroupDescription ::@ @Text@
--
-- * @SubnetIds ::@ @[Text]@
--
mkCreateCacheSubnetGroup :: Text -- ^ 'ccsg1CacheSubnetGroupName'
                         -> Text -- ^ 'ccsg1CacheSubnetGroupDescription'
                         -> [Text] -- ^ 'ccsg1SubnetIds'
                         -> CreateCacheSubnetGroup
mkCreateCacheSubnetGroup p1 p2 p3 = CreateCacheSubnetGroup
    { _ccsg1CacheSubnetGroupName = p1
    , _ccsg1CacheSubnetGroupDescription = p2
    , _ccsg1SubnetIds = p3
    }

-- | A name for the cache subnet group. This value is stored as a lowercase
-- string. Constraints: Must contain no more than 255 alphanumeric characters
-- or hyphens. Example: mysubnetgroup.
ccsg1CacheSubnetGroupName :: Lens' CreateCacheSubnetGroup Text
ccsg1CacheSubnetGroupName =
    lens _ccsg1CacheSubnetGroupName
         (\s a -> s { _ccsg1CacheSubnetGroupName = a })

-- | A description for the cache subnet group.
ccsg1CacheSubnetGroupDescription :: Lens' CreateCacheSubnetGroup Text
ccsg1CacheSubnetGroupDescription =
    lens _ccsg1CacheSubnetGroupDescription
         (\s a -> s { _ccsg1CacheSubnetGroupDescription = a })

-- | A list of VPC subnet IDs for the cache subnet group.
ccsg1SubnetIds :: Lens' CreateCacheSubnetGroup [Text]
ccsg1SubnetIds = lens _ccsg1SubnetIds (\s a -> s { _ccsg1SubnetIds = a })

instance ToQuery CreateCacheSubnetGroup where
    toQuery = genericQuery def

newtype CreateCacheSubnetGroupResponse = CreateCacheSubnetGroupResponse
    { _ccsgrrCacheSubnetGroup :: Maybe CacheSubnetGroup
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateCacheSubnetGroupResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CacheSubnetGroup ::@ @Maybe CacheSubnetGroup@
--
mkCreateCacheSubnetGroupResponse :: CreateCacheSubnetGroupResponse
mkCreateCacheSubnetGroupResponse = CreateCacheSubnetGroupResponse
    { _ccsgrrCacheSubnetGroup = Nothing
    }

-- | Represents the output of one of the following operations:
-- CreateCacheSubnetGroup ModifyCacheSubnetGroup.
ccsgrrCacheSubnetGroup :: Lens' CreateCacheSubnetGroupResponse (Maybe CacheSubnetGroup)
ccsgrrCacheSubnetGroup =
    lens _ccsgrrCacheSubnetGroup (\s a -> s { _ccsgrrCacheSubnetGroup = a })

instance FromXML CreateCacheSubnetGroupResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateCacheSubnetGroup where
    type Sv CreateCacheSubnetGroup = ElastiCache
    type Rs CreateCacheSubnetGroup = CreateCacheSubnetGroupResponse

    request = post "CreateCacheSubnetGroup"
    response _ = xmlResponse
