{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.DescribeEnvironmentResources
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns AWS resources for this environment.
--
-- /See:/ <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_DescribeEnvironmentResources.html AWS API Reference> for DescribeEnvironmentResources.
module Network.AWS.ElasticBeanstalk.DescribeEnvironmentResources
    (
    -- * Creating a Request
      describeEnvironmentResources
    , DescribeEnvironmentResources
    -- * Request Lenses
    , derEnvironmentName
    , derEnvironmentId

    -- * Destructuring the Response
    , describeEnvironmentResourcesResponse
    , DescribeEnvironmentResourcesResponse
    -- * Response Lenses
    , derrsEnvironmentResources
    , derrsStatus
    ) where

import           Network.AWS.ElasticBeanstalk.Types
import           Network.AWS.ElasticBeanstalk.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | This documentation target is not reported in the API reference.
--
-- /See:/ 'describeEnvironmentResources' smart constructor.
data DescribeEnvironmentResources = DescribeEnvironmentResources'
    { _derEnvironmentName :: !(Maybe Text)
    , _derEnvironmentId   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeEnvironmentResources' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'derEnvironmentName'
--
-- * 'derEnvironmentId'
describeEnvironmentResources
    :: DescribeEnvironmentResources
describeEnvironmentResources =
    DescribeEnvironmentResources'
    { _derEnvironmentName = Nothing
    , _derEnvironmentId = Nothing
    }

-- | The name of the environment to retrieve AWS resource usage data.
--
-- Condition: You must specify either this or an EnvironmentId, or both. If
-- you do not specify either, AWS Elastic Beanstalk returns
-- 'MissingRequiredParameter' error.
derEnvironmentName :: Lens' DescribeEnvironmentResources (Maybe Text)
derEnvironmentName = lens _derEnvironmentName (\ s a -> s{_derEnvironmentName = a});

-- | The ID of the environment to retrieve AWS resource usage data.
--
-- Condition: You must specify either this or an EnvironmentName, or both.
-- If you do not specify either, AWS Elastic Beanstalk returns
-- 'MissingRequiredParameter' error.
derEnvironmentId :: Lens' DescribeEnvironmentResources (Maybe Text)
derEnvironmentId = lens _derEnvironmentId (\ s a -> s{_derEnvironmentId = a});

instance AWSRequest DescribeEnvironmentResources
         where
        type Rs DescribeEnvironmentResources =
             DescribeEnvironmentResourcesResponse
        request = postQuery elasticBeanstalk
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
data DescribeEnvironmentResourcesResponse = DescribeEnvironmentResourcesResponse'
    { _derrsEnvironmentResources :: !(Maybe EnvironmentResourceDescription)
    , _derrsStatus               :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeEnvironmentResourcesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'derrsEnvironmentResources'
--
-- * 'derrsStatus'
describeEnvironmentResourcesResponse
    :: Int -- ^ 'derrsStatus'
    -> DescribeEnvironmentResourcesResponse
describeEnvironmentResourcesResponse pStatus_ =
    DescribeEnvironmentResourcesResponse'
    { _derrsEnvironmentResources = Nothing
    , _derrsStatus = pStatus_
    }

-- | A list of EnvironmentResourceDescription.
derrsEnvironmentResources :: Lens' DescribeEnvironmentResourcesResponse (Maybe EnvironmentResourceDescription)
derrsEnvironmentResources = lens _derrsEnvironmentResources (\ s a -> s{_derrsEnvironmentResources = a});

-- | The response status code.
derrsStatus :: Lens' DescribeEnvironmentResourcesResponse Int
derrsStatus = lens _derrsStatus (\ s a -> s{_derrsStatus = a});
