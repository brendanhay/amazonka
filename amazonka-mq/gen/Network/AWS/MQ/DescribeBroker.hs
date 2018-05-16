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
-- Module      : Network.AWS.MQ.DescribeBroker
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified broker.
module Network.AWS.MQ.DescribeBroker
    (
    -- * Creating a Request
      describeBroker
    , DescribeBroker
    -- * Request Lenses
    , desBrokerId

    -- * Destructuring the Response
    , describeBrokerResponse
    , DescribeBrokerResponse
    -- * Response Lenses
    , dbrsBrokerName
    , dbrsEngineVersion
    , dbrsBrokerState
    , dbrsPubliclyAccessible
    , dbrsAutoMinorVersionUpgrade
    , dbrsSecurityGroups
    , dbrsUsers
    , dbrsSubnetIds
    , dbrsConfigurations
    , dbrsMaintenanceWindowStartTime
    , dbrsDeploymentMode
    , dbrsBrokerId
    , dbrsEngineType
    , dbrsBrokerARN
    , dbrsBrokerInstances
    , dbrsHostInstanceType
    , dbrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MQ.Types
import Network.AWS.MQ.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeBroker' smart constructor.
newtype DescribeBroker = DescribeBroker'
  { _desBrokerId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeBroker' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desBrokerId' - The name of the broker. This value must be unique in your AWS account, 1-50 characters long, must contain only letters, numbers, dashes, and underscores, and must not contain whitespaces, brackets, wildcard characters, or special characters.
describeBroker
    :: Text -- ^ 'desBrokerId'
    -> DescribeBroker
describeBroker pBrokerId_ = DescribeBroker' {_desBrokerId = pBrokerId_}


-- | The name of the broker. This value must be unique in your AWS account, 1-50 characters long, must contain only letters, numbers, dashes, and underscores, and must not contain whitespaces, brackets, wildcard characters, or special characters.
desBrokerId :: Lens' DescribeBroker Text
desBrokerId = lens _desBrokerId (\ s a -> s{_desBrokerId = a})

instance AWSRequest DescribeBroker where
        type Rs DescribeBroker = DescribeBrokerResponse
        request = get mq
        response
          = receiveJSON
              (\ s h x ->
                 DescribeBrokerResponse' <$>
                   (x .?> "brokerName") <*> (x .?> "engineVersion") <*>
                     (x .?> "brokerState")
                     <*> (x .?> "publiclyAccessible")
                     <*> (x .?> "autoMinorVersionUpgrade")
                     <*> (x .?> "securityGroups" .!@ mempty)
                     <*> (x .?> "users" .!@ mempty)
                     <*> (x .?> "subnetIds" .!@ mempty)
                     <*> (x .?> "configurations")
                     <*> (x .?> "maintenanceWindowStartTime")
                     <*> (x .?> "deploymentMode")
                     <*> (x .?> "brokerId")
                     <*> (x .?> "engineType")
                     <*> (x .?> "brokerArn")
                     <*> (x .?> "brokerInstances" .!@ mempty)
                     <*> (x .?> "hostInstanceType")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeBroker where

instance NFData DescribeBroker where

instance ToHeaders DescribeBroker where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DescribeBroker where
        toPath DescribeBroker'{..}
          = mconcat ["/v1/brokers/", toBS _desBrokerId]

instance ToQuery DescribeBroker where
        toQuery = const mempty

-- | /See:/ 'describeBrokerResponse' smart constructor.
data DescribeBrokerResponse = DescribeBrokerResponse'
  { _dbrsBrokerName                 :: !(Maybe Text)
  , _dbrsEngineVersion              :: !(Maybe Text)
  , _dbrsBrokerState                :: !(Maybe BrokerState)
  , _dbrsPubliclyAccessible         :: !(Maybe Bool)
  , _dbrsAutoMinorVersionUpgrade    :: !(Maybe Bool)
  , _dbrsSecurityGroups             :: !(Maybe [Text])
  , _dbrsUsers                      :: !(Maybe [UserSummary])
  , _dbrsSubnetIds                  :: !(Maybe [Text])
  , _dbrsConfigurations             :: !(Maybe Configurations)
  , _dbrsMaintenanceWindowStartTime :: !(Maybe WeeklyStartTime)
  , _dbrsDeploymentMode             :: !(Maybe DeploymentMode)
  , _dbrsBrokerId                   :: !(Maybe Text)
  , _dbrsEngineType                 :: !(Maybe EngineType)
  , _dbrsBrokerARN                  :: !(Maybe Text)
  , _dbrsBrokerInstances            :: !(Maybe [BrokerInstance])
  , _dbrsHostInstanceType           :: !(Maybe Text)
  , _dbrsResponseStatus             :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeBrokerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbrsBrokerName' - The name of the broker. This value must be unique in your AWS account, 1-50 characters long, must contain only letters, numbers, dashes, and underscores, and must not contain whitespaces, brackets, wildcard characters, or special characters.
--
-- * 'dbrsEngineVersion' - The version of the broker engine. Note: Currently, Amazon MQ supports only 5.15.0.
--
-- * 'dbrsBrokerState' - The status of the broker. Possible values: CREATION_IN_PROGRESS, CREATION_FAILED, DELETION_IN_PROGRESS, RUNNING, REBOOT_IN_PROGRESS
--
-- * 'dbrsPubliclyAccessible' - Required. Enables connections from applications outside of the VPC that hosts the broker's subnets.
--
-- * 'dbrsAutoMinorVersionUpgrade' - Required. Enables automatic upgrades to new minor versions for brokers, as Apache releases the versions. The automatic upgrades occur during the maintenance window of the broker or after a manual broker reboot.
--
-- * 'dbrsSecurityGroups' - Required. The list of rules (1 minimum, 125 maximum) that authorize connections to brokers.
--
-- * 'dbrsUsers' - The list of all ActiveMQ usernames for the specified broker.
--
-- * 'dbrsSubnetIds' - The list of groups (2 maximum) that define which subnets and IP ranges the broker can use from different Availability Zones. A SINGLE_INSTANCE deployment requires one subnet (for example, the default subnet). An ACTIVE_STANDBY_MULTI_AZ deployment requires two subnets.
--
-- * 'dbrsConfigurations' - The list of all revisions for the specified configuration.
--
-- * 'dbrsMaintenanceWindowStartTime' - The parameters that determine the WeeklyStartTime.
--
-- * 'dbrsDeploymentMode' - Required. The deployment mode of the broker. Possible values: SINGLE_INSTANCE, ACTIVE_STANDBY_MULTI_AZ SINGLE_INSTANCE creates a single-instance broker in a single Availability Zone. ACTIVE_STANDBY_MULTI_AZ creates an active/standby broker for high availability.
--
-- * 'dbrsBrokerId' - The unique ID that Amazon MQ generates for the broker.
--
-- * 'dbrsEngineType' - Required. The type of broker engine. Note: Currently, Amazon MQ supports only ACTIVEMQ.
--
-- * 'dbrsBrokerARN' - The Amazon Resource Name (ARN) of the broker.
--
-- * 'dbrsBrokerInstances' - A list of information about allocated brokers.
--
-- * 'dbrsHostInstanceType' - The broker's instance type. Possible values: mq.t2.micro, mq.m4.large
--
-- * 'dbrsResponseStatus' - -- | The response status code.
describeBrokerResponse
    :: Int -- ^ 'dbrsResponseStatus'
    -> DescribeBrokerResponse
describeBrokerResponse pResponseStatus_ =
  DescribeBrokerResponse'
    { _dbrsBrokerName = Nothing
    , _dbrsEngineVersion = Nothing
    , _dbrsBrokerState = Nothing
    , _dbrsPubliclyAccessible = Nothing
    , _dbrsAutoMinorVersionUpgrade = Nothing
    , _dbrsSecurityGroups = Nothing
    , _dbrsUsers = Nothing
    , _dbrsSubnetIds = Nothing
    , _dbrsConfigurations = Nothing
    , _dbrsMaintenanceWindowStartTime = Nothing
    , _dbrsDeploymentMode = Nothing
    , _dbrsBrokerId = Nothing
    , _dbrsEngineType = Nothing
    , _dbrsBrokerARN = Nothing
    , _dbrsBrokerInstances = Nothing
    , _dbrsHostInstanceType = Nothing
    , _dbrsResponseStatus = pResponseStatus_
    }


-- | The name of the broker. This value must be unique in your AWS account, 1-50 characters long, must contain only letters, numbers, dashes, and underscores, and must not contain whitespaces, brackets, wildcard characters, or special characters.
dbrsBrokerName :: Lens' DescribeBrokerResponse (Maybe Text)
dbrsBrokerName = lens _dbrsBrokerName (\ s a -> s{_dbrsBrokerName = a})

-- | The version of the broker engine. Note: Currently, Amazon MQ supports only 5.15.0.
dbrsEngineVersion :: Lens' DescribeBrokerResponse (Maybe Text)
dbrsEngineVersion = lens _dbrsEngineVersion (\ s a -> s{_dbrsEngineVersion = a})

-- | The status of the broker. Possible values: CREATION_IN_PROGRESS, CREATION_FAILED, DELETION_IN_PROGRESS, RUNNING, REBOOT_IN_PROGRESS
dbrsBrokerState :: Lens' DescribeBrokerResponse (Maybe BrokerState)
dbrsBrokerState = lens _dbrsBrokerState (\ s a -> s{_dbrsBrokerState = a})

-- | Required. Enables connections from applications outside of the VPC that hosts the broker's subnets.
dbrsPubliclyAccessible :: Lens' DescribeBrokerResponse (Maybe Bool)
dbrsPubliclyAccessible = lens _dbrsPubliclyAccessible (\ s a -> s{_dbrsPubliclyAccessible = a})

-- | Required. Enables automatic upgrades to new minor versions for brokers, as Apache releases the versions. The automatic upgrades occur during the maintenance window of the broker or after a manual broker reboot.
dbrsAutoMinorVersionUpgrade :: Lens' DescribeBrokerResponse (Maybe Bool)
dbrsAutoMinorVersionUpgrade = lens _dbrsAutoMinorVersionUpgrade (\ s a -> s{_dbrsAutoMinorVersionUpgrade = a})

-- | Required. The list of rules (1 minimum, 125 maximum) that authorize connections to brokers.
dbrsSecurityGroups :: Lens' DescribeBrokerResponse [Text]
dbrsSecurityGroups = lens _dbrsSecurityGroups (\ s a -> s{_dbrsSecurityGroups = a}) . _Default . _Coerce

-- | The list of all ActiveMQ usernames for the specified broker.
dbrsUsers :: Lens' DescribeBrokerResponse [UserSummary]
dbrsUsers = lens _dbrsUsers (\ s a -> s{_dbrsUsers = a}) . _Default . _Coerce

-- | The list of groups (2 maximum) that define which subnets and IP ranges the broker can use from different Availability Zones. A SINGLE_INSTANCE deployment requires one subnet (for example, the default subnet). An ACTIVE_STANDBY_MULTI_AZ deployment requires two subnets.
dbrsSubnetIds :: Lens' DescribeBrokerResponse [Text]
dbrsSubnetIds = lens _dbrsSubnetIds (\ s a -> s{_dbrsSubnetIds = a}) . _Default . _Coerce

-- | The list of all revisions for the specified configuration.
dbrsConfigurations :: Lens' DescribeBrokerResponse (Maybe Configurations)
dbrsConfigurations = lens _dbrsConfigurations (\ s a -> s{_dbrsConfigurations = a})

-- | The parameters that determine the WeeklyStartTime.
dbrsMaintenanceWindowStartTime :: Lens' DescribeBrokerResponse (Maybe WeeklyStartTime)
dbrsMaintenanceWindowStartTime = lens _dbrsMaintenanceWindowStartTime (\ s a -> s{_dbrsMaintenanceWindowStartTime = a})

-- | Required. The deployment mode of the broker. Possible values: SINGLE_INSTANCE, ACTIVE_STANDBY_MULTI_AZ SINGLE_INSTANCE creates a single-instance broker in a single Availability Zone. ACTIVE_STANDBY_MULTI_AZ creates an active/standby broker for high availability.
dbrsDeploymentMode :: Lens' DescribeBrokerResponse (Maybe DeploymentMode)
dbrsDeploymentMode = lens _dbrsDeploymentMode (\ s a -> s{_dbrsDeploymentMode = a})

-- | The unique ID that Amazon MQ generates for the broker.
dbrsBrokerId :: Lens' DescribeBrokerResponse (Maybe Text)
dbrsBrokerId = lens _dbrsBrokerId (\ s a -> s{_dbrsBrokerId = a})

-- | Required. The type of broker engine. Note: Currently, Amazon MQ supports only ACTIVEMQ.
dbrsEngineType :: Lens' DescribeBrokerResponse (Maybe EngineType)
dbrsEngineType = lens _dbrsEngineType (\ s a -> s{_dbrsEngineType = a})

-- | The Amazon Resource Name (ARN) of the broker.
dbrsBrokerARN :: Lens' DescribeBrokerResponse (Maybe Text)
dbrsBrokerARN = lens _dbrsBrokerARN (\ s a -> s{_dbrsBrokerARN = a})

-- | A list of information about allocated brokers.
dbrsBrokerInstances :: Lens' DescribeBrokerResponse [BrokerInstance]
dbrsBrokerInstances = lens _dbrsBrokerInstances (\ s a -> s{_dbrsBrokerInstances = a}) . _Default . _Coerce

-- | The broker's instance type. Possible values: mq.t2.micro, mq.m4.large
dbrsHostInstanceType :: Lens' DescribeBrokerResponse (Maybe Text)
dbrsHostInstanceType = lens _dbrsHostInstanceType (\ s a -> s{_dbrsHostInstanceType = a})

-- | -- | The response status code.
dbrsResponseStatus :: Lens' DescribeBrokerResponse Int
dbrsResponseStatus = lens _dbrsResponseStatus (\ s a -> s{_dbrsResponseStatus = a})

instance NFData DescribeBrokerResponse where
