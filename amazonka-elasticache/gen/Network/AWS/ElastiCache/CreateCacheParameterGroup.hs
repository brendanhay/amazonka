{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.CreateCacheParameterGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The CreateCacheParameterGroup operation creates a new cache parameter
-- group. A cache parameter group is a collection of parameters that you apply
-- to all of the nodes in a cache cluster.
-- https://elasticache.us-east-1.amazonaws.com/
-- ?Action=CreateCacheParameterGroup
-- &Description=My%20first%20cache%20parameter%20group
-- &CacheParameterGroupFamily=memcached1.4
-- &CacheParameterGroupName=mycacheparametergroup1 &Version=2014-03-24
-- &SignatureVersion=4 &SignatureMethod=HmacSHA256 &Timestamp=20140401T192317Z
-- &X-Amz-Credential= mycacheparametergroup3 memcached1.4 My first cache
-- parameter group 05699541-b7f9-11e0-9326-b7275b9d4a6c.
module Network.AWS.ElastiCache.CreateCacheParameterGroup
    (
    -- * Request
      CreateCacheParameterGroup
    -- ** Request constructor
    , createCacheParameterGroup
    -- ** Request lenses
    , ccpgCacheParameterGroupName
    , ccpgCacheParameterGroupFamily
    , ccpgDescription

    -- * Response
    , CreateCacheParameterGroupResponse
    -- ** Response constructor
    , createCacheParameterGroupResponse
    -- ** Response lenses
    , ccpgrCacheParameterGroup
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types
import Network.AWS.Prelude

-- | Represents the input of a CreateCacheParameterGroup operation.
data CreateCacheParameterGroup = CreateCacheParameterGroup
    { _ccpgCacheParameterGroupName :: Text
    , _ccpgCacheParameterGroupFamily :: Text
    , _ccpgDescription :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateCacheParameterGroup' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CacheParameterGroupName ::@ @Text@
--
-- * @CacheParameterGroupFamily ::@ @Text@
--
-- * @Description ::@ @Text@
--
createCacheParameterGroup :: Text -- ^ 'ccpgCacheParameterGroupName'
                            -> Text -- ^ 'ccpgCacheParameterGroupFamily'
                            -> Text -- ^ 'ccpgDescription'
                            -> CreateCacheParameterGroup
createCacheParameterGroup p1 p2 p3 = CreateCacheParameterGroup
    { _ccpgCacheParameterGroupName = p1
    , _ccpgCacheParameterGroupFamily = p2
    , _ccpgDescription = p3
    }

-- | A user-specified name for the cache parameter group.
ccpgCacheParameterGroupName :: Lens' CreateCacheParameterGroup Text
ccpgCacheParameterGroupName =
    lens _ccpgCacheParameterGroupName
         (\s a -> s { _ccpgCacheParameterGroupName = a })

-- | The name of the cache parameter group family the cache parameter group can
-- be used with. Valid values are: memcached1.4 | redis2.6 | redis2.8.
ccpgCacheParameterGroupFamily :: Lens' CreateCacheParameterGroup Text
ccpgCacheParameterGroupFamily =
    lens _ccpgCacheParameterGroupFamily
         (\s a -> s { _ccpgCacheParameterGroupFamily = a })

-- | A user-specified description for the cache parameter group.
ccpgDescription :: Lens' CreateCacheParameterGroup Text
ccpgDescription = lens _ccpgDescription (\s a -> s { _ccpgDescription = a })

instance ToQuery CreateCacheParameterGroup where
    toQuery = genericQuery def

newtype CreateCacheParameterGroupResponse = CreateCacheParameterGroupResponse
    { _ccpgrCacheParameterGroup :: Maybe CacheParameterGroup
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateCacheParameterGroupResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CacheParameterGroup ::@ @Maybe CacheParameterGroup@
--
createCacheParameterGroupResponse :: CreateCacheParameterGroupResponse
createCacheParameterGroupResponse = CreateCacheParameterGroupResponse
    { _ccpgrCacheParameterGroup = Nothing
    }

-- | Represents the output of a CreateCacheParameterGroup operation.
ccpgrCacheParameterGroup :: Lens' CreateCacheParameterGroupResponse (Maybe CacheParameterGroup)
ccpgrCacheParameterGroup =
    lens _ccpgrCacheParameterGroup
         (\s a -> s { _ccpgrCacheParameterGroup = a })

instance FromXML CreateCacheParameterGroupResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateCacheParameterGroup where
    type Sv CreateCacheParameterGroup = ElastiCache
    type Rs CreateCacheParameterGroup = CreateCacheParameterGroupResponse

    request = post "CreateCacheParameterGroup"
    response _ = xmlResponse
