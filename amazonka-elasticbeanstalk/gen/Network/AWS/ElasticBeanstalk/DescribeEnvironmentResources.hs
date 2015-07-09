{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.DescribeEnvironmentResources
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
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
    , derrStatus
    ) where

import           Network.AWS.ElasticBeanstalk.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | This documentation target is not reported in the API reference.
--
-- /See:/ 'describeEnvironmentResources' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'derEnvironmentName'
--
-- * 'derEnvironmentId'
data DescribeEnvironmentResources = DescribeEnvironmentResources'
    { _derEnvironmentName :: !(Maybe Text)
    , _derEnvironmentId   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeEnvironmentResources' smart constructor.
describeEnvironmentResources :: DescribeEnvironmentResources
describeEnvironmentResources =
    DescribeEnvironmentResources'
    { _derEnvironmentName = Nothing
    , _derEnvironmentId = Nothing
    }

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
                   (x .@? "EnvironmentResources") <*>
                     (pure (fromEnum s)))

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

-- | Result message containing a list of environment resource descriptions.
--
-- /See:/ 'describeEnvironmentResourcesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'derrEnvironmentResources'
--
-- * 'derrStatus'
data DescribeEnvironmentResourcesResponse = DescribeEnvironmentResourcesResponse'
    { _derrEnvironmentResources :: !(Maybe EnvironmentResourceDescription)
    , _derrStatus               :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeEnvironmentResourcesResponse' smart constructor.
describeEnvironmentResourcesResponse :: Int -> DescribeEnvironmentResourcesResponse
describeEnvironmentResourcesResponse pStatus =
    DescribeEnvironmentResourcesResponse'
    { _derrEnvironmentResources = Nothing
    , _derrStatus = pStatus
    }

-- | A list of EnvironmentResourceDescription.
derrEnvironmentResources :: Lens' DescribeEnvironmentResourcesResponse (Maybe EnvironmentResourceDescription)
derrEnvironmentResources = lens _derrEnvironmentResources (\ s a -> s{_derrEnvironmentResources = a});

-- | FIXME: Undocumented member.
derrStatus :: Lens' DescribeEnvironmentResourcesResponse Int
derrStatus = lens _derrStatus (\ s a -> s{_derrStatus = a});
