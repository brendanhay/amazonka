{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.DescribeEnvironmentResources
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns AWS resources for this environment.
--
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_DescribeEnvironmentResources.html>
module Network.AWS.ElasticBeanstalk.DescribeEnvironmentResources
    (
    -- * Request
      DescribeEnvironmentResources
    -- ** Request constructor
    , describeEnvironmentResources
    -- ** Request lenses
    , derEnvironmentId
    , derEnvironmentName

    -- * Response
    , DescribeEnvironmentResourcesResponse
    -- ** Response constructor
    , describeEnvironmentResourcesResponse
    -- ** Response lenses
    , derrEnvironmentResources
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types
import qualified GHC.Exts

data DescribeEnvironmentResources = DescribeEnvironmentResources
    { _derEnvironmentId   :: Maybe Text
    , _derEnvironmentName :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeEnvironmentResources' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'derEnvironmentId' @::@ 'Maybe' 'Text'
--
-- * 'derEnvironmentName' @::@ 'Maybe' 'Text'
--
describeEnvironmentResources :: DescribeEnvironmentResources
describeEnvironmentResources = DescribeEnvironmentResources
    { _derEnvironmentId   = Nothing
    , _derEnvironmentName = Nothing
    }

-- | The ID of the environment to retrieve AWS resource usage data. Condition:
-- You must specify either this or an EnvironmentName, or both. If you do
-- not specify either, AWS Elastic Beanstalk returns
-- MissingRequiredParameter error.
derEnvironmentId :: Lens' DescribeEnvironmentResources (Maybe Text)
derEnvironmentId = lens _derEnvironmentId (\s a -> s { _derEnvironmentId = a })

-- | The name of the environment to retrieve AWS resource usage data.
-- Condition: You must specify either this or an EnvironmentId, or both. If
-- you do not specify either, AWS Elastic Beanstalk returns
-- MissingRequiredParameter error.
derEnvironmentName :: Lens' DescribeEnvironmentResources (Maybe Text)
derEnvironmentName =
    lens _derEnvironmentName (\s a -> s { _derEnvironmentName = a })

newtype DescribeEnvironmentResourcesResponse = DescribeEnvironmentResourcesResponse
    { _derrEnvironmentResources :: Maybe EnvironmentResourceDescription
    } deriving (Eq, Show, Generic)

-- | 'DescribeEnvironmentResourcesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'derrEnvironmentResources' @::@ 'Maybe' 'EnvironmentResourceDescription'
--
describeEnvironmentResourcesResponse :: DescribeEnvironmentResourcesResponse
describeEnvironmentResourcesResponse = DescribeEnvironmentResourcesResponse
    { _derrEnvironmentResources = Nothing
    }

-- | A list of EnvironmentResourceDescription.
derrEnvironmentResources :: Lens' DescribeEnvironmentResourcesResponse (Maybe EnvironmentResourceDescription)
derrEnvironmentResources =
    lens _derrEnvironmentResources
        (\s a -> s { _derrEnvironmentResources = a })

instance ToPath DescribeEnvironmentResources where
    toPath = const "/"

instance ToQuery DescribeEnvironmentResources

instance ToHeaders DescribeEnvironmentResources

instance AWSRequest DescribeEnvironmentResources where
    type Sv DescribeEnvironmentResources = ElasticBeanstalk
    type Rs DescribeEnvironmentResources = DescribeEnvironmentResourcesResponse

    request  = post "DescribeEnvironmentResources"
    response = xmlResponse

instance FromXML DescribeEnvironmentResourcesResponse where
    parseXML c = DescribeEnvironmentResourcesResponse
        <$> c .:? "EnvironmentResources"
