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
-- Module      : Network.AWS.MQ.CreateBroker
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a broker. Note: This API is asynchronous.
module Network.AWS.MQ.CreateBroker
    (
    -- * Creating a Request
      createBroker
    , CreateBroker
    -- * Request Lenses
    , cbBrokerName
    , cbEngineVersion
    , cbPubliclyAccessible
    , cbAutoMinorVersionUpgrade
    , cbSecurityGroups
    , cbUsers
    , cbSubnetIds
    , cbCreatorRequestId
    , cbMaintenanceWindowStartTime
    , cbDeploymentMode
    , cbConfiguration
    , cbEngineType
    , cbHostInstanceType

    -- * Destructuring the Response
    , createBrokerResponse
    , CreateBrokerResponse
    -- * Response Lenses
    , cbrsBrokerId
    , cbrsBrokerARN
    , cbrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MQ.Types
import Network.AWS.MQ.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Creates a broker using the specified properties.
--
-- /See:/ 'createBroker' smart constructor.
data CreateBroker = CreateBroker'
  { _cbBrokerName                 :: !(Maybe Text)
  , _cbEngineVersion              :: !(Maybe Text)
  , _cbPubliclyAccessible         :: !(Maybe Bool)
  , _cbAutoMinorVersionUpgrade    :: !(Maybe Bool)
  , _cbSecurityGroups             :: !(Maybe [Text])
  , _cbUsers                      :: !(Maybe [User])
  , _cbSubnetIds                  :: !(Maybe [Text])
  , _cbCreatorRequestId           :: !(Maybe Text)
  , _cbMaintenanceWindowStartTime :: !(Maybe WeeklyStartTime)
  , _cbDeploymentMode             :: !(Maybe DeploymentMode)
  , _cbConfiguration              :: !(Maybe ConfigurationId)
  , _cbEngineType                 :: !(Maybe EngineType)
  , _cbHostInstanceType           :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateBroker' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbBrokerName' - Required. The name of the broker. This value must be unique in your AWS account, 1-50 characters long, must contain only letters, numbers, dashes, and underscores, and must not contain whitespaces, brackets, wildcard characters, or special characters.
--
-- * 'cbEngineVersion' - Required. The version of the broker engine. Note: Currently, Amazon MQ supports only 5.15.0.
--
-- * 'cbPubliclyAccessible' - Required. Enables connections from applications outside of the VPC that hosts the broker's subnets.
--
-- * 'cbAutoMinorVersionUpgrade' - Required. Enables automatic upgrades to new minor versions for brokers, as Apache releases the versions. The automatic upgrades occur during the maintenance window of the broker or after a manual broker reboot.
--
-- * 'cbSecurityGroups' - Required. The list of rules (1 minimum, 125 maximum) that authorize connections to brokers.
--
-- * 'cbUsers' - Required. The list of ActiveMQ users (persons or applications) who can access queues and topics. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
--
-- * 'cbSubnetIds' - Required. The list of groups (2 maximum) that define which subnets and IP ranges the broker can use from different Availability Zones. A SINGLE_INSTANCE deployment requires one subnet (for example, the default subnet). An ACTIVE_STANDBY_MULTI_AZ deployment requires two subnets.
--
-- * 'cbCreatorRequestId' - The unique ID that the requester receives for the created broker. Amazon MQ passes your ID with the API action. Note: We recommend using a Universally Unique Identifier (UUID) for the creatorRequestId. You may omit the creatorRequestId if your application doesn't require idempotency.
--
-- * 'cbMaintenanceWindowStartTime' - The parameters that determine the WeeklyStartTime.
--
-- * 'cbDeploymentMode' - Required. The deployment mode of the broker. Possible values: SINGLE_INSTANCE, ACTIVE_STANDBY_MULTI_AZ SINGLE_INSTANCE creates a single-instance broker in a single Availability Zone. ACTIVE_STANDBY_MULTI_AZ creates an active/standby broker for high availability.
--
-- * 'cbConfiguration' - A list of information about the configuration.
--
-- * 'cbEngineType' - Required. The type of broker engine. Note: Currently, Amazon MQ supports only ACTIVEMQ.
--
-- * 'cbHostInstanceType' - Required. The broker's instance type. Possible values: mq.t2.micro, mq.m4.large
createBroker
    :: CreateBroker
createBroker =
  CreateBroker'
    { _cbBrokerName = Nothing
    , _cbEngineVersion = Nothing
    , _cbPubliclyAccessible = Nothing
    , _cbAutoMinorVersionUpgrade = Nothing
    , _cbSecurityGroups = Nothing
    , _cbUsers = Nothing
    , _cbSubnetIds = Nothing
    , _cbCreatorRequestId = Nothing
    , _cbMaintenanceWindowStartTime = Nothing
    , _cbDeploymentMode = Nothing
    , _cbConfiguration = Nothing
    , _cbEngineType = Nothing
    , _cbHostInstanceType = Nothing
    }


-- | Required. The name of the broker. This value must be unique in your AWS account, 1-50 characters long, must contain only letters, numbers, dashes, and underscores, and must not contain whitespaces, brackets, wildcard characters, or special characters.
cbBrokerName :: Lens' CreateBroker (Maybe Text)
cbBrokerName = lens _cbBrokerName (\ s a -> s{_cbBrokerName = a})

-- | Required. The version of the broker engine. Note: Currently, Amazon MQ supports only 5.15.0.
cbEngineVersion :: Lens' CreateBroker (Maybe Text)
cbEngineVersion = lens _cbEngineVersion (\ s a -> s{_cbEngineVersion = a})

-- | Required. Enables connections from applications outside of the VPC that hosts the broker's subnets.
cbPubliclyAccessible :: Lens' CreateBroker (Maybe Bool)
cbPubliclyAccessible = lens _cbPubliclyAccessible (\ s a -> s{_cbPubliclyAccessible = a})

-- | Required. Enables automatic upgrades to new minor versions for brokers, as Apache releases the versions. The automatic upgrades occur during the maintenance window of the broker or after a manual broker reboot.
cbAutoMinorVersionUpgrade :: Lens' CreateBroker (Maybe Bool)
cbAutoMinorVersionUpgrade = lens _cbAutoMinorVersionUpgrade (\ s a -> s{_cbAutoMinorVersionUpgrade = a})

-- | Required. The list of rules (1 minimum, 125 maximum) that authorize connections to brokers.
cbSecurityGroups :: Lens' CreateBroker [Text]
cbSecurityGroups = lens _cbSecurityGroups (\ s a -> s{_cbSecurityGroups = a}) . _Default . _Coerce

-- | Required. The list of ActiveMQ users (persons or applications) who can access queues and topics. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
cbUsers :: Lens' CreateBroker [User]
cbUsers = lens _cbUsers (\ s a -> s{_cbUsers = a}) . _Default . _Coerce

-- | Required. The list of groups (2 maximum) that define which subnets and IP ranges the broker can use from different Availability Zones. A SINGLE_INSTANCE deployment requires one subnet (for example, the default subnet). An ACTIVE_STANDBY_MULTI_AZ deployment requires two subnets.
cbSubnetIds :: Lens' CreateBroker [Text]
cbSubnetIds = lens _cbSubnetIds (\ s a -> s{_cbSubnetIds = a}) . _Default . _Coerce

-- | The unique ID that the requester receives for the created broker. Amazon MQ passes your ID with the API action. Note: We recommend using a Universally Unique Identifier (UUID) for the creatorRequestId. You may omit the creatorRequestId if your application doesn't require idempotency.
cbCreatorRequestId :: Lens' CreateBroker (Maybe Text)
cbCreatorRequestId = lens _cbCreatorRequestId (\ s a -> s{_cbCreatorRequestId = a})

-- | The parameters that determine the WeeklyStartTime.
cbMaintenanceWindowStartTime :: Lens' CreateBroker (Maybe WeeklyStartTime)
cbMaintenanceWindowStartTime = lens _cbMaintenanceWindowStartTime (\ s a -> s{_cbMaintenanceWindowStartTime = a})

-- | Required. The deployment mode of the broker. Possible values: SINGLE_INSTANCE, ACTIVE_STANDBY_MULTI_AZ SINGLE_INSTANCE creates a single-instance broker in a single Availability Zone. ACTIVE_STANDBY_MULTI_AZ creates an active/standby broker for high availability.
cbDeploymentMode :: Lens' CreateBroker (Maybe DeploymentMode)
cbDeploymentMode = lens _cbDeploymentMode (\ s a -> s{_cbDeploymentMode = a})

-- | A list of information about the configuration.
cbConfiguration :: Lens' CreateBroker (Maybe ConfigurationId)
cbConfiguration = lens _cbConfiguration (\ s a -> s{_cbConfiguration = a})

-- | Required. The type of broker engine. Note: Currently, Amazon MQ supports only ACTIVEMQ.
cbEngineType :: Lens' CreateBroker (Maybe EngineType)
cbEngineType = lens _cbEngineType (\ s a -> s{_cbEngineType = a})

-- | Required. The broker's instance type. Possible values: mq.t2.micro, mq.m4.large
cbHostInstanceType :: Lens' CreateBroker (Maybe Text)
cbHostInstanceType = lens _cbHostInstanceType (\ s a -> s{_cbHostInstanceType = a})

instance AWSRequest CreateBroker where
        type Rs CreateBroker = CreateBrokerResponse
        request = postJSON mq
        response
          = receiveJSON
              (\ s h x ->
                 CreateBrokerResponse' <$>
                   (x .?> "brokerId") <*> (x .?> "brokerArn") <*>
                     (pure (fromEnum s)))

instance Hashable CreateBroker where

instance NFData CreateBroker where

instance ToHeaders CreateBroker where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateBroker where
        toJSON CreateBroker'{..}
          = object
              (catMaybes
                 [("brokerName" .=) <$> _cbBrokerName,
                  ("engineVersion" .=) <$> _cbEngineVersion,
                  ("publiclyAccessible" .=) <$> _cbPubliclyAccessible,
                  ("autoMinorVersionUpgrade" .=) <$>
                    _cbAutoMinorVersionUpgrade,
                  ("securityGroups" .=) <$> _cbSecurityGroups,
                  ("users" .=) <$> _cbUsers,
                  ("subnetIds" .=) <$> _cbSubnetIds,
                  ("creatorRequestId" .=) <$> _cbCreatorRequestId,
                  ("maintenanceWindowStartTime" .=) <$>
                    _cbMaintenanceWindowStartTime,
                  ("deploymentMode" .=) <$> _cbDeploymentMode,
                  ("configuration" .=) <$> _cbConfiguration,
                  ("engineType" .=) <$> _cbEngineType,
                  ("hostInstanceType" .=) <$> _cbHostInstanceType])

instance ToPath CreateBroker where
        toPath = const "/v1/brokers"

instance ToQuery CreateBroker where
        toQuery = const mempty

-- | /See:/ 'createBrokerResponse' smart constructor.
data CreateBrokerResponse = CreateBrokerResponse'
  { _cbrsBrokerId       :: !(Maybe Text)
  , _cbrsBrokerARN      :: !(Maybe Text)
  , _cbrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateBrokerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbrsBrokerId' - The unique ID that Amazon MQ generates for the broker.
--
-- * 'cbrsBrokerARN' - The Amazon Resource Name (ARN) of the broker.
--
-- * 'cbrsResponseStatus' - -- | The response status code.
createBrokerResponse
    :: Int -- ^ 'cbrsResponseStatus'
    -> CreateBrokerResponse
createBrokerResponse pResponseStatus_ =
  CreateBrokerResponse'
    { _cbrsBrokerId = Nothing
    , _cbrsBrokerARN = Nothing
    , _cbrsResponseStatus = pResponseStatus_
    }


-- | The unique ID that Amazon MQ generates for the broker.
cbrsBrokerId :: Lens' CreateBrokerResponse (Maybe Text)
cbrsBrokerId = lens _cbrsBrokerId (\ s a -> s{_cbrsBrokerId = a})

-- | The Amazon Resource Name (ARN) of the broker.
cbrsBrokerARN :: Lens' CreateBrokerResponse (Maybe Text)
cbrsBrokerARN = lens _cbrsBrokerARN (\ s a -> s{_cbrsBrokerARN = a})

-- | -- | The response status code.
cbrsResponseStatus :: Lens' CreateBrokerResponse Int
cbrsResponseStatus = lens _cbrsResponseStatus (\ s a -> s{_cbrsResponseStatus = a})

instance NFData CreateBrokerResponse where
