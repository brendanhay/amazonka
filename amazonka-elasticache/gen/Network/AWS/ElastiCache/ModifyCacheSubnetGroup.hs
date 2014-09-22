{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.ModifyCacheSubnetGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The ModifyCacheSubnetGroup operation modifies an existing cache subnet
-- group. https://elasticache.amazonaws.com/ ?Action=ModifyCacheSubnetGroup
-- &CacheSubnetGroupName=myCachesubnetgroup
-- &CacheSubnetGroupDescription=My%20modified%20CacheSubnetGroup
-- &Version=2014-03-24 &SignatureVersion=4 &SignatureMethod=HmacSHA256
-- &Timestamp=20140401T192317Z &X-Amz-Credential= 990524496922 My modified
-- CacheSubnetGroup myCachesubnetgroup Active subnet-7c5b4115 us-east-1c
-- Active subnet-7b5b4112 us-east-1b Active subnet-3ea6bd57 us-east-1d
-- ed662948-a57b-11df-9e38-7ffab86c801f.
module Network.AWS.ElastiCache.ModifyCacheSubnetGroup
    (
    -- * Request
      ModifyCacheSubnetGroup
    -- ** Request constructor
    , modifyCacheSubnetGroup
    -- ** Request lenses
    , mcsgCacheSubnetGroupName
    , mcsgCacheSubnetGroupDescription
    , mcsgSubnetIdentifier

    -- * Response
    , ModifyCacheSubnetGroupResponse
    -- ** Response constructor
    , modifyCacheSubnetGroupResponse
    -- ** Response lenses
    , mcsgrCacheSubnetGroup
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types
import Network.AWS.Prelude

-- | Represents the input of a ModifyCacheSubnetGroup operation.
data ModifyCacheSubnetGroup = ModifyCacheSubnetGroup
    { _mcsgCacheSubnetGroupName :: Text
    , _mcsgCacheSubnetGroupDescription :: Maybe Text
    , _mcsgSubnetIdentifier :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ModifyCacheSubnetGroup' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CacheSubnetGroupName ::@ @Text@
--
-- * @CacheSubnetGroupDescription ::@ @Maybe Text@
--
-- * @SubnetIdentifier ::@ @[Text]@
--
modifyCacheSubnetGroup :: Text -- ^ 'mcsgCacheSubnetGroupName'
                       -> ModifyCacheSubnetGroup
modifyCacheSubnetGroup p1 = ModifyCacheSubnetGroup
    { _mcsgCacheSubnetGroupName = p1
    , _mcsgCacheSubnetGroupDescription = Nothing
    , _mcsgSubnetIdentifier = mempty
    }

-- | The name for the cache subnet group. This value is stored as a lowercase
-- string. Constraints: Must contain no more than 255 alphanumeric characters
-- or hyphens. Example: mysubnetgroup.
mcsgCacheSubnetGroupName :: Lens' ModifyCacheSubnetGroup Text
mcsgCacheSubnetGroupName =
    lens _mcsgCacheSubnetGroupName
         (\s a -> s { _mcsgCacheSubnetGroupName = a })

-- | A description for the cache subnet group.
mcsgCacheSubnetGroupDescription :: Lens' ModifyCacheSubnetGroup (Maybe Text)
mcsgCacheSubnetGroupDescription =
    lens _mcsgCacheSubnetGroupDescription
         (\s a -> s { _mcsgCacheSubnetGroupDescription = a })

-- | The EC2 subnet IDs for the cache subnet group.
mcsgSubnetIdentifier :: Lens' ModifyCacheSubnetGroup [Text]
mcsgSubnetIdentifier =
    lens _mcsgSubnetIdentifier (\s a -> s { _mcsgSubnetIdentifier = a })

instance ToQuery ModifyCacheSubnetGroup where
    toQuery = genericQuery def

newtype ModifyCacheSubnetGroupResponse = ModifyCacheSubnetGroupResponse
    { _mcsgrCacheSubnetGroup :: Maybe CacheSubnetGroup
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ModifyCacheSubnetGroupResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CacheSubnetGroup ::@ @Maybe CacheSubnetGroup@
--
modifyCacheSubnetGroupResponse :: ModifyCacheSubnetGroupResponse
modifyCacheSubnetGroupResponse = ModifyCacheSubnetGroupResponse
    { _mcsgrCacheSubnetGroup = Nothing
    }

-- | Represents the output of one of the following operations:
-- CreateCacheSubnetGroup ModifyCacheSubnetGroup.
mcsgrCacheSubnetGroup :: Lens' ModifyCacheSubnetGroupResponse (Maybe CacheSubnetGroup)
mcsgrCacheSubnetGroup =
    lens _mcsgrCacheSubnetGroup (\s a -> s { _mcsgrCacheSubnetGroup = a })

instance FromXML ModifyCacheSubnetGroupResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ModifyCacheSubnetGroup where
    type Sv ModifyCacheSubnetGroup = ElastiCache
    type Rs ModifyCacheSubnetGroup = ModifyCacheSubnetGroupResponse

    request = post "ModifyCacheSubnetGroup"
    response _ = xmlResponse
