{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ElasticBeanstalk.DescribeEnvironmentResources
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

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
    , derEnvironmentName
    , derEnvironmentId

    -- * Response
    , DescribeEnvironmentResourcesResponse
    -- ** Response constructor
    , describeEnvironmentResourcesResponse
    -- ** Response lenses
    , derrEnvironmentResources
    ) where

import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeEnvironmentResources' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'derEnvironmentName'
--
-- * 'derEnvironmentId'
data DescribeEnvironmentResources = DescribeEnvironmentResources'{_derEnvironmentName :: Maybe Text, _derEnvironmentId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DescribeEnvironmentResources' smart constructor.
describeEnvironmentResources :: DescribeEnvironmentResources
describeEnvironmentResources = DescribeEnvironmentResources'{_derEnvironmentName = Nothing, _derEnvironmentId = Nothing};

-- | The name of the environment to retrieve AWS resource usage data.
--
-- Condition: You must specify either this or an EnvironmentId, or both. If
-- you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
derEnvironmentName :: Lens' DescribeEnvironmentResources (Maybe Text)
derEnvironmentName = lens _derEnvironmentName (\ s a -> s{_derEnvironmentName = a});

-- | The ID of the environment to retrieve AWS resource usage data.
--
-- Condition: You must specify either this or an EnvironmentName, or both.
-- If you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
derEnvironmentId :: Lens' DescribeEnvironmentResources (Maybe Text)
derEnvironmentId = lens _derEnvironmentId (\ s a -> s{_derEnvironmentId = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest DescribeEnvironmentResources
         where
        type Sv DescribeEnvironmentResources =
             ElasticBeanstalk
        type Rs DescribeEnvironmentResources =
             DescribeEnvironmentResourcesResponse
        request = post
        response
          = receiveXMLWrapper
              "DescribeEnvironmentResourcesResult"
              (\ s h x ->
                 DescribeEnvironmentResourcesResponse' <$>
                   (x .@? "EnvironmentResources"))

instance ToHeaders DescribeEnvironmentResources where
        toHeaders = const mempty

instance ToPath DescribeEnvironmentResources where
        toPath = const "/"

instance ToQuery DescribeEnvironmentResources where
        toQuery DescribeEnvironmentResources'{..}
          = mconcat
              ["Action" =:
                 ("DescribeEnvironmentResources" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "EnvironmentName" =: _derEnvironmentName,
               "EnvironmentId" =: _derEnvironmentId]

-- | /See:/ 'describeEnvironmentResourcesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'derrEnvironmentResources'
newtype DescribeEnvironmentResourcesResponse = DescribeEnvironmentResourcesResponse'{_derrEnvironmentResources :: Maybe EnvironmentResourceDescription} deriving (Eq, Read, Show)

-- | 'DescribeEnvironmentResourcesResponse' smart constructor.
describeEnvironmentResourcesResponse :: DescribeEnvironmentResourcesResponse
describeEnvironmentResourcesResponse = DescribeEnvironmentResourcesResponse'{_derrEnvironmentResources = Nothing};

-- | A list of EnvironmentResourceDescription.
derrEnvironmentResources :: Lens' DescribeEnvironmentResourcesResponse (Maybe EnvironmentResourceDescription)
derrEnvironmentResources = lens _derrEnvironmentResources (\ s a -> s{_derrEnvironmentResources = a});
