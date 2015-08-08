{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.TerminateEnvironment
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Terminates the specified environment.
--
-- /See:/ <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_TerminateEnvironment.html AWS API Reference> for TerminateEnvironment.
module Network.AWS.ElasticBeanstalk.TerminateEnvironment
    (
    -- * Creating a Request
      TerminateEnvironment
    , terminateEnvironment
    -- * Request Lenses
    , teTerminateResources
    , teEnvironmentName
    , teEnvironmentId

    -- * Destructuring the Response
    , EnvironmentDescription
    , environmentDescription
    -- * Response Lenses
    , eCNAME
    , eStatus
    , eTemplateName
    , eAbortableOperationInProgress
    , eEndpointURL
    , eDateUpdated
    , eResources
    , eHealth
    , eVersionLabel
    , eDateCreated
    , eTier
    , eEnvironmentName
    , eApplicationName
    , eEnvironmentId
    , eSolutionStackName
    , eDescription
    ) where

import           Network.AWS.ElasticBeanstalk.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

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
data TerminateEnvironment = TerminateEnvironment'
    { _teTerminateResources :: !(Maybe Bool)
    , _teEnvironmentName    :: !(Maybe Text)
    , _teEnvironmentId      :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'TerminateEnvironment' smart constructor.
terminateEnvironment :: TerminateEnvironment
terminateEnvironment =
    TerminateEnvironment'
    { _teTerminateResources = Nothing
    , _teEnvironmentName = Nothing
    , _teEnvironmentId = Nothing
    }

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
        request = postQuery
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
