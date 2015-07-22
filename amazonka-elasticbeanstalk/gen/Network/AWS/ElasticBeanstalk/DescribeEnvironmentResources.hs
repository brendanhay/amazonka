{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.DescribeEnvironmentResources
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns AWS resources for this environment.
--
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_DescribeEnvironmentResources.html>
module Network.AWS.ElasticBeanstalk.DescribeEnvironmentResources
    (
    -- * Request
      DescribeEnvironmentResources
    -- ** Request constructor
    , describeEnvironmentResources
    -- ** Request lenses
    , derrqEnvironmentName
    , derrqEnvironmentId

    -- * Response
    , DescribeEnvironmentResourcesResponse
    -- ** Response constructor
    , describeEnvironmentResourcesResponse
    -- ** Response lenses
    , derrsEnvironmentResources
    , derrsStatus
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
-- * 'derrqEnvironmentName'
--
-- * 'derrqEnvironmentId'
data DescribeEnvironmentResources = DescribeEnvironmentResources'
    { _derrqEnvironmentName :: !(Maybe Text)
    , _derrqEnvironmentId   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeEnvironmentResources' smart constructor.
describeEnvironmentResources :: DescribeEnvironmentResources
describeEnvironmentResources =
    DescribeEnvironmentResources'
    { _derrqEnvironmentName = Nothing
    , _derrqEnvironmentId = Nothing
    }

-- | The name of the environment to retrieve AWS resource usage data.
--
-- Condition: You must specify either this or an EnvironmentId, or both. If
-- you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
derrqEnvironmentName :: Lens' DescribeEnvironmentResources (Maybe Text)
derrqEnvironmentName = lens _derrqEnvironmentName (\ s a -> s{_derrqEnvironmentName = a});

-- | The ID of the environment to retrieve AWS resource usage data.
--
-- Condition: You must specify either this or an EnvironmentName, or both.
-- If you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
derrqEnvironmentId :: Lens' DescribeEnvironmentResources (Maybe Text)
derrqEnvironmentId = lens _derrqEnvironmentId (\ s a -> s{_derrqEnvironmentId = a});

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
               "EnvironmentName" =: _derrqEnvironmentName,
               "EnvironmentId" =: _derrqEnvironmentId]

-- | Result message containing a list of environment resource descriptions.
--
-- /See:/ 'describeEnvironmentResourcesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'derrsEnvironmentResources'
--
-- * 'derrsStatus'
data DescribeEnvironmentResourcesResponse = DescribeEnvironmentResourcesResponse'
    { _derrsEnvironmentResources :: !(Maybe EnvironmentResourceDescription)
    , _derrsStatus               :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeEnvironmentResourcesResponse' smart constructor.
describeEnvironmentResourcesResponse :: Int -> DescribeEnvironmentResourcesResponse
describeEnvironmentResourcesResponse pStatus =
    DescribeEnvironmentResourcesResponse'
    { _derrsEnvironmentResources = Nothing
    , _derrsStatus = pStatus
    }

-- | A list of EnvironmentResourceDescription.
derrsEnvironmentResources :: Lens' DescribeEnvironmentResourcesResponse (Maybe EnvironmentResourceDescription)
derrsEnvironmentResources = lens _derrsEnvironmentResources (\ s a -> s{_derrsEnvironmentResources = a});

-- | FIXME: Undocumented member.
derrsStatus :: Lens' DescribeEnvironmentResourcesResponse Int
derrsStatus = lens _derrsStatus (\ s a -> s{_derrsStatus = a});
