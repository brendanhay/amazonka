{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.DescribeBroker
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified broker.
module Network.AWS.MQ.DescribeBroker
  ( -- * Creating a Request
    describeBroker,
    DescribeBroker,

    -- * Request Lenses
    desBrokerId,

    -- * Destructuring the Response
    describeBrokerResponse,
    DescribeBrokerResponse,

    -- * Response Lenses
    dbrsBrokerName,
    dbrsEngineVersion,
    dbrsPendingAuthenticationStrategy,
    dbrsBrokerState,
    dbrsPubliclyAccessible,
    dbrsAutoMinorVersionUpgrade,
    dbrsSecurityGroups,
    dbrsUsers,
    dbrsPendingSecurityGroups,
    dbrsSubnetIds,
    dbrsCreated,
    dbrsConfigurations,
    dbrsAuthenticationStrategy,
    dbrsPendingHostInstanceType,
    dbrsLdapServerMetadata,
    dbrsMaintenanceWindowStartTime,
    dbrsLogs,
    dbrsEncryptionOptions,
    dbrsDeploymentMode,
    dbrsPendingEngineVersion,
    dbrsBrokerId,
    dbrsPendingLdapServerMetadata,
    dbrsEngineType,
    dbrsBrokerARN,
    dbrsTags,
    dbrsBrokerInstances,
    dbrsHostInstanceType,
    dbrsStorageType,
    dbrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MQ.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeBroker' smart constructor.
newtype DescribeBroker = DescribeBroker' {_desBrokerId :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeBroker' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desBrokerId' - The name of the broker. This value must be unique in your AWS account, 1-50 characters long, must contain only letters, numbers, dashes, and underscores, and must not contain whitespaces, brackets, wildcard characters, or special characters.
describeBroker ::
  -- | 'desBrokerId'
  Text ->
  DescribeBroker
describeBroker pBrokerId_ =
  DescribeBroker' {_desBrokerId = pBrokerId_}

-- | The name of the broker. This value must be unique in your AWS account, 1-50 characters long, must contain only letters, numbers, dashes, and underscores, and must not contain whitespaces, brackets, wildcard characters, or special characters.
desBrokerId :: Lens' DescribeBroker Text
desBrokerId = lens _desBrokerId (\s a -> s {_desBrokerId = a})

instance AWSRequest DescribeBroker where
  type Rs DescribeBroker = DescribeBrokerResponse
  request = get mq
  response =
    receiveJSON
      ( \s h x ->
          DescribeBrokerResponse'
            <$> (x .?> "brokerName")
            <*> (x .?> "engineVersion")
            <*> (x .?> "pendingAuthenticationStrategy")
            <*> (x .?> "brokerState")
            <*> (x .?> "publiclyAccessible")
            <*> (x .?> "autoMinorVersionUpgrade")
            <*> (x .?> "securityGroups" .!@ mempty)
            <*> (x .?> "users" .!@ mempty)
            <*> (x .?> "pendingSecurityGroups" .!@ mempty)
            <*> (x .?> "subnetIds" .!@ mempty)
            <*> (x .?> "created")
            <*> (x .?> "configurations")
            <*> (x .?> "authenticationStrategy")
            <*> (x .?> "pendingHostInstanceType")
            <*> (x .?> "ldapServerMetadata")
            <*> (x .?> "maintenanceWindowStartTime")
            <*> (x .?> "logs")
            <*> (x .?> "encryptionOptions")
            <*> (x .?> "deploymentMode")
            <*> (x .?> "pendingEngineVersion")
            <*> (x .?> "brokerId")
            <*> (x .?> "pendingLdapServerMetadata")
            <*> (x .?> "engineType")
            <*> (x .?> "brokerArn")
            <*> (x .?> "tags" .!@ mempty)
            <*> (x .?> "brokerInstances" .!@ mempty)
            <*> (x .?> "hostInstanceType")
            <*> (x .?> "storageType")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeBroker

instance NFData DescribeBroker

instance ToHeaders DescribeBroker where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath DescribeBroker where
  toPath DescribeBroker' {..} =
    mconcat ["/v1/brokers/", toBS _desBrokerId]

instance ToQuery DescribeBroker where
  toQuery = const mempty

-- | /See:/ 'describeBrokerResponse' smart constructor.
data DescribeBrokerResponse = DescribeBrokerResponse'
  { _dbrsBrokerName ::
      !(Maybe Text),
    _dbrsEngineVersion :: !(Maybe Text),
    _dbrsPendingAuthenticationStrategy ::
      !(Maybe AuthenticationStrategy),
    _dbrsBrokerState :: !(Maybe BrokerState),
    _dbrsPubliclyAccessible :: !(Maybe Bool),
    _dbrsAutoMinorVersionUpgrade :: !(Maybe Bool),
    _dbrsSecurityGroups :: !(Maybe [Text]),
    _dbrsUsers :: !(Maybe [UserSummary]),
    _dbrsPendingSecurityGroups :: !(Maybe [Text]),
    _dbrsSubnetIds :: !(Maybe [Text]),
    _dbrsCreated :: !(Maybe POSIX),
    _dbrsConfigurations ::
      !(Maybe Configurations),
    _dbrsAuthenticationStrategy ::
      !(Maybe AuthenticationStrategy),
    _dbrsPendingHostInstanceType :: !(Maybe Text),
    _dbrsLdapServerMetadata ::
      !(Maybe LdapServerMetadataOutput),
    _dbrsMaintenanceWindowStartTime ::
      !(Maybe WeeklyStartTime),
    _dbrsLogs :: !(Maybe LogsSummary),
    _dbrsEncryptionOptions ::
      !(Maybe EncryptionOptions),
    _dbrsDeploymentMode ::
      !(Maybe DeploymentMode),
    _dbrsPendingEngineVersion :: !(Maybe Text),
    _dbrsBrokerId :: !(Maybe Text),
    _dbrsPendingLdapServerMetadata ::
      !(Maybe LdapServerMetadataOutput),
    _dbrsEngineType :: !(Maybe EngineType),
    _dbrsBrokerARN :: !(Maybe Text),
    _dbrsTags :: !(Maybe (Map Text (Text))),
    _dbrsBrokerInstances ::
      !(Maybe [BrokerInstance]),
    _dbrsHostInstanceType :: !(Maybe Text),
    _dbrsStorageType ::
      !(Maybe BrokerStorageType),
    _dbrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeBrokerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbrsBrokerName' - The name of the broker. This value must be unique in your AWS account, 1-50 characters long, must contain only letters, numbers, dashes, and underscores, and must not contain whitespaces, brackets, wildcard characters, or special characters.
--
-- * 'dbrsEngineVersion' - The version of the broker engine. For a list of supported engine versions, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide/broker-engine.html
--
-- * 'dbrsPendingAuthenticationStrategy' - The authentication strategy that will be applied when the broker is rebooted.
--
-- * 'dbrsBrokerState' - The status of the broker.
--
-- * 'dbrsPubliclyAccessible' - Required. Enables connections from applications outside of the VPC that hosts the broker's subnets.
--
-- * 'dbrsAutoMinorVersionUpgrade' - Required. Enables automatic upgrades to new minor versions for brokers, as Apache releases the versions. The automatic upgrades occur during the maintenance window of the broker or after a manual broker reboot.
--
-- * 'dbrsSecurityGroups' - The list of security groups (1 minimum, 5 maximum) that authorizes connections to brokers.
--
-- * 'dbrsUsers' - The list of all broker usernames for the specified broker.
--
-- * 'dbrsPendingSecurityGroups' - The list of pending security groups to authorize connections to brokers.
--
-- * 'dbrsSubnetIds' - The list of groups that define which subnets and IP ranges the broker can use from different Availability Zones. A SINGLE_INSTANCE deployment requires one subnet (for example, the default subnet). An ACTIVE_STANDBY_MULTI_AZ deployment (ACTIVEMQ) requires two subnets. A CLUSTER_MULTI_AZ deployment (RABBITMQ) has no subnet requirements when deployed with public accessibility, deployment without public accessibility requires at least one subnet.
--
-- * 'dbrsCreated' - The time when the broker was created.
--
-- * 'dbrsConfigurations' - The list of all revisions for the specified configuration.
--
-- * 'dbrsAuthenticationStrategy' - The authentication strategy used to secure the broker.
--
-- * 'dbrsPendingHostInstanceType' - The host instance type of the broker to upgrade to. For a list of supported instance types, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide//broker.html#broker-instance-types
--
-- * 'dbrsLdapServerMetadata' - The metadata of the LDAP server used to authenticate and authorize connections to the broker.
--
-- * 'dbrsMaintenanceWindowStartTime' - The parameters that determine the WeeklyStartTime.
--
-- * 'dbrsLogs' - The list of information about logs currently enabled and pending to be deployed for the specified broker.
--
-- * 'dbrsEncryptionOptions' - Encryption options for the broker.
--
-- * 'dbrsDeploymentMode' - Required. The deployment mode of the broker.
--
-- * 'dbrsPendingEngineVersion' - The version of the broker engine to upgrade to. For a list of supported engine versions, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide/broker-engine.html
--
-- * 'dbrsBrokerId' - The unique ID that Amazon MQ generates for the broker.
--
-- * 'dbrsPendingLdapServerMetadata' - The metadata of the LDAP server that will be used to authenticate and authorize connections to the broker once it is rebooted.
--
-- * 'dbrsEngineType' - Required. The type of broker engine. Note: Currently, Amazon MQ supports ACTIVEMQ and RABBITMQ.
--
-- * 'dbrsBrokerARN' - The Amazon Resource Name (ARN) of the broker.
--
-- * 'dbrsTags' - The list of all tags associated with this broker.
--
-- * 'dbrsBrokerInstances' - A list of information about allocated brokers.
--
-- * 'dbrsHostInstanceType' - The broker's instance type.
--
-- * 'dbrsStorageType' - The broker's storage type.
--
-- * 'dbrsResponseStatus' - -- | The response status code.
describeBrokerResponse ::
  -- | 'dbrsResponseStatus'
  Int ->
  DescribeBrokerResponse
describeBrokerResponse pResponseStatus_ =
  DescribeBrokerResponse'
    { _dbrsBrokerName = Nothing,
      _dbrsEngineVersion = Nothing,
      _dbrsPendingAuthenticationStrategy = Nothing,
      _dbrsBrokerState = Nothing,
      _dbrsPubliclyAccessible = Nothing,
      _dbrsAutoMinorVersionUpgrade = Nothing,
      _dbrsSecurityGroups = Nothing,
      _dbrsUsers = Nothing,
      _dbrsPendingSecurityGroups = Nothing,
      _dbrsSubnetIds = Nothing,
      _dbrsCreated = Nothing,
      _dbrsConfigurations = Nothing,
      _dbrsAuthenticationStrategy = Nothing,
      _dbrsPendingHostInstanceType = Nothing,
      _dbrsLdapServerMetadata = Nothing,
      _dbrsMaintenanceWindowStartTime = Nothing,
      _dbrsLogs = Nothing,
      _dbrsEncryptionOptions = Nothing,
      _dbrsDeploymentMode = Nothing,
      _dbrsPendingEngineVersion = Nothing,
      _dbrsBrokerId = Nothing,
      _dbrsPendingLdapServerMetadata = Nothing,
      _dbrsEngineType = Nothing,
      _dbrsBrokerARN = Nothing,
      _dbrsTags = Nothing,
      _dbrsBrokerInstances = Nothing,
      _dbrsHostInstanceType = Nothing,
      _dbrsStorageType = Nothing,
      _dbrsResponseStatus = pResponseStatus_
    }

-- | The name of the broker. This value must be unique in your AWS account, 1-50 characters long, must contain only letters, numbers, dashes, and underscores, and must not contain whitespaces, brackets, wildcard characters, or special characters.
dbrsBrokerName :: Lens' DescribeBrokerResponse (Maybe Text)
dbrsBrokerName = lens _dbrsBrokerName (\s a -> s {_dbrsBrokerName = a})

-- | The version of the broker engine. For a list of supported engine versions, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide/broker-engine.html
dbrsEngineVersion :: Lens' DescribeBrokerResponse (Maybe Text)
dbrsEngineVersion = lens _dbrsEngineVersion (\s a -> s {_dbrsEngineVersion = a})

-- | The authentication strategy that will be applied when the broker is rebooted.
dbrsPendingAuthenticationStrategy :: Lens' DescribeBrokerResponse (Maybe AuthenticationStrategy)
dbrsPendingAuthenticationStrategy = lens _dbrsPendingAuthenticationStrategy (\s a -> s {_dbrsPendingAuthenticationStrategy = a})

-- | The status of the broker.
dbrsBrokerState :: Lens' DescribeBrokerResponse (Maybe BrokerState)
dbrsBrokerState = lens _dbrsBrokerState (\s a -> s {_dbrsBrokerState = a})

-- | Required. Enables connections from applications outside of the VPC that hosts the broker's subnets.
dbrsPubliclyAccessible :: Lens' DescribeBrokerResponse (Maybe Bool)
dbrsPubliclyAccessible = lens _dbrsPubliclyAccessible (\s a -> s {_dbrsPubliclyAccessible = a})

-- | Required. Enables automatic upgrades to new minor versions for brokers, as Apache releases the versions. The automatic upgrades occur during the maintenance window of the broker or after a manual broker reboot.
dbrsAutoMinorVersionUpgrade :: Lens' DescribeBrokerResponse (Maybe Bool)
dbrsAutoMinorVersionUpgrade = lens _dbrsAutoMinorVersionUpgrade (\s a -> s {_dbrsAutoMinorVersionUpgrade = a})

-- | The list of security groups (1 minimum, 5 maximum) that authorizes connections to brokers.
dbrsSecurityGroups :: Lens' DescribeBrokerResponse [Text]
dbrsSecurityGroups = lens _dbrsSecurityGroups (\s a -> s {_dbrsSecurityGroups = a}) . _Default . _Coerce

-- | The list of all broker usernames for the specified broker.
dbrsUsers :: Lens' DescribeBrokerResponse [UserSummary]
dbrsUsers = lens _dbrsUsers (\s a -> s {_dbrsUsers = a}) . _Default . _Coerce

-- | The list of pending security groups to authorize connections to brokers.
dbrsPendingSecurityGroups :: Lens' DescribeBrokerResponse [Text]
dbrsPendingSecurityGroups = lens _dbrsPendingSecurityGroups (\s a -> s {_dbrsPendingSecurityGroups = a}) . _Default . _Coerce

-- | The list of groups that define which subnets and IP ranges the broker can use from different Availability Zones. A SINGLE_INSTANCE deployment requires one subnet (for example, the default subnet). An ACTIVE_STANDBY_MULTI_AZ deployment (ACTIVEMQ) requires two subnets. A CLUSTER_MULTI_AZ deployment (RABBITMQ) has no subnet requirements when deployed with public accessibility, deployment without public accessibility requires at least one subnet.
dbrsSubnetIds :: Lens' DescribeBrokerResponse [Text]
dbrsSubnetIds = lens _dbrsSubnetIds (\s a -> s {_dbrsSubnetIds = a}) . _Default . _Coerce

-- | The time when the broker was created.
dbrsCreated :: Lens' DescribeBrokerResponse (Maybe UTCTime)
dbrsCreated = lens _dbrsCreated (\s a -> s {_dbrsCreated = a}) . mapping _Time

-- | The list of all revisions for the specified configuration.
dbrsConfigurations :: Lens' DescribeBrokerResponse (Maybe Configurations)
dbrsConfigurations = lens _dbrsConfigurations (\s a -> s {_dbrsConfigurations = a})

-- | The authentication strategy used to secure the broker.
dbrsAuthenticationStrategy :: Lens' DescribeBrokerResponse (Maybe AuthenticationStrategy)
dbrsAuthenticationStrategy = lens _dbrsAuthenticationStrategy (\s a -> s {_dbrsAuthenticationStrategy = a})

-- | The host instance type of the broker to upgrade to. For a list of supported instance types, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide//broker.html#broker-instance-types
dbrsPendingHostInstanceType :: Lens' DescribeBrokerResponse (Maybe Text)
dbrsPendingHostInstanceType = lens _dbrsPendingHostInstanceType (\s a -> s {_dbrsPendingHostInstanceType = a})

-- | The metadata of the LDAP server used to authenticate and authorize connections to the broker.
dbrsLdapServerMetadata :: Lens' DescribeBrokerResponse (Maybe LdapServerMetadataOutput)
dbrsLdapServerMetadata = lens _dbrsLdapServerMetadata (\s a -> s {_dbrsLdapServerMetadata = a})

-- | The parameters that determine the WeeklyStartTime.
dbrsMaintenanceWindowStartTime :: Lens' DescribeBrokerResponse (Maybe WeeklyStartTime)
dbrsMaintenanceWindowStartTime = lens _dbrsMaintenanceWindowStartTime (\s a -> s {_dbrsMaintenanceWindowStartTime = a})

-- | The list of information about logs currently enabled and pending to be deployed for the specified broker.
dbrsLogs :: Lens' DescribeBrokerResponse (Maybe LogsSummary)
dbrsLogs = lens _dbrsLogs (\s a -> s {_dbrsLogs = a})

-- | Encryption options for the broker.
dbrsEncryptionOptions :: Lens' DescribeBrokerResponse (Maybe EncryptionOptions)
dbrsEncryptionOptions = lens _dbrsEncryptionOptions (\s a -> s {_dbrsEncryptionOptions = a})

-- | Required. The deployment mode of the broker.
dbrsDeploymentMode :: Lens' DescribeBrokerResponse (Maybe DeploymentMode)
dbrsDeploymentMode = lens _dbrsDeploymentMode (\s a -> s {_dbrsDeploymentMode = a})

-- | The version of the broker engine to upgrade to. For a list of supported engine versions, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide/broker-engine.html
dbrsPendingEngineVersion :: Lens' DescribeBrokerResponse (Maybe Text)
dbrsPendingEngineVersion = lens _dbrsPendingEngineVersion (\s a -> s {_dbrsPendingEngineVersion = a})

-- | The unique ID that Amazon MQ generates for the broker.
dbrsBrokerId :: Lens' DescribeBrokerResponse (Maybe Text)
dbrsBrokerId = lens _dbrsBrokerId (\s a -> s {_dbrsBrokerId = a})

-- | The metadata of the LDAP server that will be used to authenticate and authorize connections to the broker once it is rebooted.
dbrsPendingLdapServerMetadata :: Lens' DescribeBrokerResponse (Maybe LdapServerMetadataOutput)
dbrsPendingLdapServerMetadata = lens _dbrsPendingLdapServerMetadata (\s a -> s {_dbrsPendingLdapServerMetadata = a})

-- | Required. The type of broker engine. Note: Currently, Amazon MQ supports ACTIVEMQ and RABBITMQ.
dbrsEngineType :: Lens' DescribeBrokerResponse (Maybe EngineType)
dbrsEngineType = lens _dbrsEngineType (\s a -> s {_dbrsEngineType = a})

-- | The Amazon Resource Name (ARN) of the broker.
dbrsBrokerARN :: Lens' DescribeBrokerResponse (Maybe Text)
dbrsBrokerARN = lens _dbrsBrokerARN (\s a -> s {_dbrsBrokerARN = a})

-- | The list of all tags associated with this broker.
dbrsTags :: Lens' DescribeBrokerResponse (HashMap Text (Text))
dbrsTags = lens _dbrsTags (\s a -> s {_dbrsTags = a}) . _Default . _Map

-- | A list of information about allocated brokers.
dbrsBrokerInstances :: Lens' DescribeBrokerResponse [BrokerInstance]
dbrsBrokerInstances = lens _dbrsBrokerInstances (\s a -> s {_dbrsBrokerInstances = a}) . _Default . _Coerce

-- | The broker's instance type.
dbrsHostInstanceType :: Lens' DescribeBrokerResponse (Maybe Text)
dbrsHostInstanceType = lens _dbrsHostInstanceType (\s a -> s {_dbrsHostInstanceType = a})

-- | The broker's storage type.
dbrsStorageType :: Lens' DescribeBrokerResponse (Maybe BrokerStorageType)
dbrsStorageType = lens _dbrsStorageType (\s a -> s {_dbrsStorageType = a})

-- | -- | The response status code.
dbrsResponseStatus :: Lens' DescribeBrokerResponse Int
dbrsResponseStatus = lens _dbrsResponseStatus (\s a -> s {_dbrsResponseStatus = a})

instance NFData DescribeBrokerResponse
