{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.ElastiCache.CreateCacheSecurityGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The CreateCacheSecurityGroup operation creates a new cache security group.
-- Use a cache security group to control access to one or more cache clusters.
-- Cache security groups are only used when you are creating a cache cluster
-- outside of an Amazon Virtual Private Cloud (VPC). If you are creating a
-- cache cluster inside of a VPC, use a cache subnet group instead. For more
-- information, see CreateCacheSubnetGroup.
module Network.AWS.ElastiCache.CreateCacheSecurityGroup
    (
    -- * Request
      CreateCacheSecurityGroupMessage
    -- ** Request constructor
    , createCacheSecurityGroupMessage
    -- ** Request lenses
    , ccsgmCacheSecurityGroupName
    , ccsgmDescription

    -- * Response
    , CreateCacheSecurityGroupResult
    -- ** Response constructor
    , createCacheSecurityGroupResult
    -- ** Response lenses
    , ccsgrCacheSecurityGroup
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types

data CreateCacheSecurityGroupMessage = CreateCacheSecurityGroupMessage
    { _ccsgmCacheSecurityGroupName :: Text
    , _ccsgmDescription            :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'CreateCacheSecurityGroupMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccsgmCacheSecurityGroupName' @::@ 'Text'
--
-- * 'ccsgmDescription' @::@ 'Text'
--
createCacheSecurityGroupMessage :: Text -- ^ 'ccsgmCacheSecurityGroupName'
                                -> Text -- ^ 'ccsgmDescription'
                                -> CreateCacheSecurityGroupMessage
createCacheSecurityGroupMessage p1 p2 = CreateCacheSecurityGroupMessage
    { _ccsgmCacheSecurityGroupName = p1
    , _ccsgmDescription            = p2
    }

-- | A name for the cache security group. This value is stored as a lowercase
-- string. Constraints: Must contain no more than 255 alphanumeric
-- characters. Cannot be the word "Default". Example: mysecuritygroup.
ccsgmCacheSecurityGroupName :: Lens' CreateCacheSecurityGroupMessage Text
ccsgmCacheSecurityGroupName =
    lens _ccsgmCacheSecurityGroupName
        (\s a -> s { _ccsgmCacheSecurityGroupName = a })

-- | A description for the cache security group.
ccsgmDescription :: Lens' CreateCacheSecurityGroupMessage Text
ccsgmDescription = lens _ccsgmDescription (\s a -> s { _ccsgmDescription = a })

instance ToPath CreateCacheSecurityGroupMessage where
    toPath = const "/"

instance ToQuery CreateCacheSecurityGroupMessage

newtype CreateCacheSecurityGroupResult = CreateCacheSecurityGroupResult
    { _ccsgrCacheSecurityGroup :: Maybe CacheSecurityGroup
    } deriving (Eq, Show, Generic)

-- | 'CreateCacheSecurityGroupResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccsgrCacheSecurityGroup' @::@ 'Maybe' 'CacheSecurityGroup'
--
createCacheSecurityGroupResult :: CreateCacheSecurityGroupResult
createCacheSecurityGroupResult = CreateCacheSecurityGroupResult
    { _ccsgrCacheSecurityGroup = Nothing
    }

ccsgrCacheSecurityGroup :: Lens' CreateCacheSecurityGroupResult (Maybe CacheSecurityGroup)
ccsgrCacheSecurityGroup =
    lens _ccsgrCacheSecurityGroup (\s a -> s { _ccsgrCacheSecurityGroup = a })

instance AWSRequest CreateCacheSecurityGroupMessage where
    type Sv CreateCacheSecurityGroupMessage = ElastiCache
    type Rs CreateCacheSecurityGroupMessage = CreateCacheSecurityGroupResult

    request  = post "CreateCacheSecurityGroup"
    response = const . xmlResponse $ \h x -> CreateCacheSecurityGroupResult
newtype
