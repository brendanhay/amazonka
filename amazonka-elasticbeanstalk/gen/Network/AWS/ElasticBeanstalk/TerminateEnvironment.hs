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
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_TerminateEnvironment.html>
module Network.AWS.ElasticBeanstalk.TerminateEnvironment
    (
    -- * Request
      TerminateEnvironment
    -- ** Request constructor
    , terminateEnvironment
    -- ** Request lenses
    , terqTerminateResources
    , terqEnvironmentName
    , terqEnvironmentId

    -- * Response
    , EnvironmentDescription
    -- ** Response constructor
    , environmentDescription
    -- ** Response lenses
    , tersCNAME
    , tersStatus
    , tersTemplateName
    , tersAbortableOperationInProgress
    , tersEndpointURL
    , tersDateUpdated
    , tersResources
    , tersHealth
    , tersVersionLabel
    , tersDateCreated
    , tersTier
    , tersEnvironmentName
    , tersApplicationName
    , tersEnvironmentId
    , tersSolutionStackName
    , tersDescription
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
-- * 'terqTerminateResources'
--
-- * 'terqEnvironmentName'
--
-- * 'terqEnvironmentId'
data TerminateEnvironment = TerminateEnvironment'
    { _terqTerminateResources :: !(Maybe Bool)
    , _terqEnvironmentName    :: !(Maybe Text)
    , _terqEnvironmentId      :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'TerminateEnvironment' smart constructor.
terminateEnvironment :: TerminateEnvironment
terminateEnvironment =
    TerminateEnvironment'
    { _terqTerminateResources = Nothing
    , _terqEnvironmentName = Nothing
    , _terqEnvironmentId = Nothing
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
terqTerminateResources :: Lens' TerminateEnvironment (Maybe Bool)
terqTerminateResources = lens _terqTerminateResources (\ s a -> s{_terqTerminateResources = a});

-- | The name of the environment to terminate.
--
-- Condition: You must specify either this or an EnvironmentId, or both. If
-- you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
terqEnvironmentName :: Lens' TerminateEnvironment (Maybe Text)
terqEnvironmentName = lens _terqEnvironmentName (\ s a -> s{_terqEnvironmentName = a});

-- | The ID of the environment to terminate.
--
-- Condition: You must specify either this or an EnvironmentName, or both.
-- If you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
terqEnvironmentId :: Lens' TerminateEnvironment (Maybe Text)
terqEnvironmentId = lens _terqEnvironmentId (\ s a -> s{_terqEnvironmentId = a});

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
               "TerminateResources" =: _terqTerminateResources,
               "EnvironmentName" =: _terqEnvironmentName,
               "EnvironmentId" =: _terqEnvironmentId]
