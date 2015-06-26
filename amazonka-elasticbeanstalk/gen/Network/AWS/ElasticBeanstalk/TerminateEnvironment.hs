{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ElasticBeanstalk.TerminateEnvironment
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

-- | Terminates the specified environment.
--
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_TerminateEnvironment.html>
module Network.AWS.ElasticBeanstalk.TerminateEnvironment
    (
    -- * Request
      TerminateEnvironment
    -- ** Request constructor
    , terminateEnvironment
    -- ** Request lenses
    , teTerminateResources
    , teEnvironmentName
    , teEnvironmentId

    -- * Response
    , EnvironmentDescription
    -- ** Response constructor
    , environmentDescription
    -- ** Response lenses
    , envCNAME
    , envStatus
    , envTemplateName
    , envAbortableOperationInProgress
    , envEndpointURL
    , envDateUpdated
    , envResources
    , envHealth
    , envVersionLabel
    , envDateCreated
    , envTier
    , envEnvironmentName
    , envApplicationName
    , envEnvironmentId
    , envSolutionStackName
    , envDescription
    ) where

import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | This documentation target is not reported in the API reference.
--
-- /See:/ 'terminateEnvironment' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'teTerminateResources'
--
-- * 'teEnvironmentName'
--
-- * 'teEnvironmentId'
data TerminateEnvironment = TerminateEnvironment'{_teTerminateResources :: Maybe Bool, _teEnvironmentName :: Maybe Text, _teEnvironmentId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'TerminateEnvironment' smart constructor.
terminateEnvironment :: TerminateEnvironment
terminateEnvironment = TerminateEnvironment'{_teTerminateResources = Nothing, _teEnvironmentName = Nothing, _teEnvironmentId = Nothing};

-- | Indicates whether the associated AWS resources should shut down when the
-- environment is terminated:
--
-- @true@: (default) The user AWS resources (for example, the Auto Scaling
-- group, LoadBalancer, etc.) are terminated along with the environment.
--
-- @false@: The environment is removed from the AWS Elastic Beanstalk but
-- the AWS resources continue to operate.
--
-- -   @true@: The specified environment as well as the associated AWS
--     resources, such as Auto Scaling group and LoadBalancer, are
--     terminated.
-- -   @false@: AWS Elastic Beanstalk resource management is removed from
--     the environment, but the AWS resources continue to operate.
--
-- For more information, see the
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/ug/ AWS Elastic Beanstalk User Guide.>
--
-- Default: @true@
--
-- Valid Values: @true@ | @false@
teTerminateResources :: Lens' TerminateEnvironment (Maybe Bool)
teTerminateResources = lens _teTerminateResources (\ s a -> s{_teTerminateResources = a});

-- | The name of the environment to terminate.
--
-- Condition: You must specify either this or an EnvironmentId, or both. If
-- you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
teEnvironmentName :: Lens' TerminateEnvironment (Maybe Text)
teEnvironmentName = lens _teEnvironmentName (\ s a -> s{_teEnvironmentName = a});

-- | The ID of the environment to terminate.
--
-- Condition: You must specify either this or an EnvironmentName, or both.
-- If you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
teEnvironmentId :: Lens' TerminateEnvironment (Maybe Text)
teEnvironmentId = lens _teEnvironmentId (\ s a -> s{_teEnvironmentId = a});

instance AWSRequest TerminateEnvironment where
        type Sv TerminateEnvironment = ElasticBeanstalk
        type Rs TerminateEnvironment = EnvironmentDescription
        request = post
        response
          = receiveXMLWrapper "TerminateEnvironmentResult"
              (\ s h x -> parseXML x)

instance ToHeaders TerminateEnvironment where
        toHeaders = const mempty

instance ToPath TerminateEnvironment where
        toPath = const "/"

instance ToQuery TerminateEnvironment where
        toQuery TerminateEnvironment'{..}
          = mconcat
              ["Action" =: ("TerminateEnvironment" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "TerminateResources" =: _teTerminateResources,
               "EnvironmentName" =: _teEnvironmentName,
               "EnvironmentId" =: _teEnvironmentId]
