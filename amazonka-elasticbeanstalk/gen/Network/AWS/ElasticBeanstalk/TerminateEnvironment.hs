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
-- Module      : Network.AWS.ElasticBeanstalk.TerminateEnvironment
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Terminates the specified environment.
--
--
module Network.AWS.ElasticBeanstalk.TerminateEnvironment
    (
    -- * Creating a Request
      terminateEnvironment
    , TerminateEnvironment
    -- * Request Lenses
    , teForceTerminate
    , teTerminateResources
    , teEnvironmentName
    , teEnvironmentId

    -- * Destructuring the Response
    , environmentDescription
    , EnvironmentDescription
    -- * Response Lenses
    , eStatus
    , eCNAME
    , eTemplateName
    , eAbortableOperationInProgress
    , eEndpointURL
    , eResources
    , eDateUpdated
    , eDateCreated
    , eHealth
    , eVersionLabel
    , ePlatformARN
    , eTier
    , eEnvironmentName
    , eApplicationName
    , eEnvironmentARN
    , eSolutionStackName
    , eEnvironmentId
    , eHealthStatus
    , eEnvironmentLinks
    , eDescription
    ) where

import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.ElasticBeanstalk.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request to terminate an environment.
--
--
--
-- /See:/ 'terminateEnvironment' smart constructor.
data TerminateEnvironment = TerminateEnvironment'
  { _teForceTerminate     :: !(Maybe Bool)
  , _teTerminateResources :: !(Maybe Bool)
  , _teEnvironmentName    :: !(Maybe Text)
  , _teEnvironmentId      :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TerminateEnvironment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'teForceTerminate' - Terminates the target environment even if another environment in the same group is dependent on it.
--
-- * 'teTerminateResources' - Indicates whether the associated AWS resources should shut down when the environment is terminated:     * @true@ : The specified environment as well as the associated AWS resources, such as Auto Scaling group and LoadBalancer, are terminated.     * @false@ : AWS Elastic Beanstalk resource management is removed from the environment, but the AWS resources continue to operate. For more information, see the <http://docs.aws.amazon.com/elasticbeanstalk/latest/ug/ AWS Elastic Beanstalk User Guide. >  Default: @true@  Valid Values: @true@ | @false@
--
-- * 'teEnvironmentName' - The name of the environment to terminate. Condition: You must specify either this or an EnvironmentId, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
--
-- * 'teEnvironmentId' - The ID of the environment to terminate. Condition: You must specify either this or an EnvironmentName, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
terminateEnvironment
    :: TerminateEnvironment
terminateEnvironment =
  TerminateEnvironment'
    { _teForceTerminate = Nothing
    , _teTerminateResources = Nothing
    , _teEnvironmentName = Nothing
    , _teEnvironmentId = Nothing
    }


-- | Terminates the target environment even if another environment in the same group is dependent on it.
teForceTerminate :: Lens' TerminateEnvironment (Maybe Bool)
teForceTerminate = lens _teForceTerminate (\ s a -> s{_teForceTerminate = a})

-- | Indicates whether the associated AWS resources should shut down when the environment is terminated:     * @true@ : The specified environment as well as the associated AWS resources, such as Auto Scaling group and LoadBalancer, are terminated.     * @false@ : AWS Elastic Beanstalk resource management is removed from the environment, but the AWS resources continue to operate. For more information, see the <http://docs.aws.amazon.com/elasticbeanstalk/latest/ug/ AWS Elastic Beanstalk User Guide. >  Default: @true@  Valid Values: @true@ | @false@
teTerminateResources :: Lens' TerminateEnvironment (Maybe Bool)
teTerminateResources = lens _teTerminateResources (\ s a -> s{_teTerminateResources = a})

-- | The name of the environment to terminate. Condition: You must specify either this or an EnvironmentId, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
teEnvironmentName :: Lens' TerminateEnvironment (Maybe Text)
teEnvironmentName = lens _teEnvironmentName (\ s a -> s{_teEnvironmentName = a})

-- | The ID of the environment to terminate. Condition: You must specify either this or an EnvironmentName, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
teEnvironmentId :: Lens' TerminateEnvironment (Maybe Text)
teEnvironmentId = lens _teEnvironmentId (\ s a -> s{_teEnvironmentId = a})

instance AWSRequest TerminateEnvironment where
        type Rs TerminateEnvironment = EnvironmentDescription
        request = postQuery elasticBeanstalk
        response
          = receiveXMLWrapper "TerminateEnvironmentResult"
              (\ s h x -> parseXML x)

instance Hashable TerminateEnvironment where

instance NFData TerminateEnvironment where

instance ToHeaders TerminateEnvironment where
        toHeaders = const mempty

instance ToPath TerminateEnvironment where
        toPath = const "/"

instance ToQuery TerminateEnvironment where
        toQuery TerminateEnvironment'{..}
          = mconcat
              ["Action" =: ("TerminateEnvironment" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "ForceTerminate" =: _teForceTerminate,
               "TerminateResources" =: _teTerminateResources,
               "EnvironmentName" =: _teEnvironmentName,
               "EnvironmentId" =: _teEnvironmentId]
