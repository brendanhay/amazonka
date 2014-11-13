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

-- Module      : Network.AWS.OpsWorks.DescribeElasticIps
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes Elastic IP addresses. Required Permissions: To use this action,
-- an IAM user must have a Show, Deploy, or Manage permissions level for the
-- stack, or an attached policy that explicitly grants permissions. For more
-- information on user permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.DescribeElasticIps
    (
    -- * Request
      DescribeElasticIps
    -- ** Request constructor
    , describeElasticIps
    -- ** Request lenses
    , deiInstanceId
    , deiIps
    , deiStackId

    -- * Response
    , DescribeElasticIpsResponse
    -- ** Response constructor
    , describeElasticIpsResponse
    -- ** Response lenses
    , deirElasticIps
    ) where

import Data.Aeson
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.OpsWorks.Types

data DescribeElasticIps = DescribeElasticIps
    { _deiInstanceId :: Maybe Text
    , _deiIps        :: [Text]
    , _deiStackId    :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeElasticIps' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'deiInstanceId' @::@ 'Maybe' 'Text'
--
-- * 'deiIps' @::@ ['Text']
--
-- * 'deiStackId' @::@ 'Maybe' 'Text'
--
describeElasticIps :: DescribeElasticIps
describeElasticIps = DescribeElasticIps
    { _deiInstanceId = Nothing
    , _deiStackId    = Nothing
    , _deiIps        = mempty
    }

-- | The instance ID. If you include this parameter, DescribeElasticIps
-- returns a description of the Elastic IP addresses associated with the
-- specified instance.
deiInstanceId :: Lens' DescribeElasticIps (Maybe Text)
deiInstanceId = lens _deiInstanceId (\s a -> s { _deiInstanceId = a })

-- | An array of Elastic IP addresses to be described. If you include this
-- parameter, DescribeElasticIps returns a description of the specified
-- Elastic IP addresses. Otherwise, it returns a description of every
-- Elastic IP address.
deiIps :: Lens' DescribeElasticIps [Text]
deiIps = lens _deiIps (\s a -> s { _deiIps = a })

-- | A stack ID. If you include this parameter, DescribeElasticIps returns a
-- description of the Elastic IP addresses that are registered with the
-- specified stack.
deiStackId :: Lens' DescribeElasticIps (Maybe Text)
deiStackId = lens _deiStackId (\s a -> s { _deiStackId = a })

instance ToPath DescribeElasticIps where
    toPath = const "/"

instance ToQuery DescribeElasticIps where
    toQuery = const mempty

instance ToHeaders DescribeElasticIps

instance ToBody DescribeElasticIps where
    toBody = toBody . encode . _deiInstanceId

newtype DescribeElasticIpsResponse = DescribeElasticIpsResponse
    { _deirElasticIps :: [ElasticIp]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeElasticIpsResponse where
    type Item DescribeElasticIpsResponse = ElasticIp

    fromList = DescribeElasticIpsResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _deirElasticIps

-- | 'DescribeElasticIpsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'deirElasticIps' @::@ ['ElasticIp']
--
describeElasticIpsResponse :: DescribeElasticIpsResponse
describeElasticIpsResponse = DescribeElasticIpsResponse
    { _deirElasticIps = mempty
    }

-- | An ElasticIps object that describes the specified Elastic IP addresses.
deirElasticIps :: Lens' DescribeElasticIpsResponse [ElasticIp]
deirElasticIps = lens _deirElasticIps (\s a -> s { _deirElasticIps = a })

-- FromJSON

instance AWSRequest DescribeElasticIps where
    type Sv DescribeElasticIps = OpsWorks
    type Rs DescribeElasticIps = DescribeElasticIpsResponse

    request  = post'
    response = jsonResponse $ \h o -> DescribeElasticIpsResponse
        <$> o .: "ElasticIps"
