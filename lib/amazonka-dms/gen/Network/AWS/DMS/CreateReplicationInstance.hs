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
-- Module      : Network.AWS.DMS.CreateReplicationInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates the replication instance using the specified parameters.
--
--
-- AWS DMS requires that your account have certain roles with appropriate permissions before you can create a replication instance. For information on the required roles, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Security.html#CHAP_Security.APIRole Creating the IAM Roles to Use With the AWS CLI and AWS DMS API> . For information on the required permissions, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Security.html#CHAP_Security.IAMPermissions IAM Permissions Needed to Use AWS DMS> .
module Network.AWS.DMS.CreateReplicationInstance
  ( -- * Creating a Request
    createReplicationInstance,
    CreateReplicationInstance,

    -- * Request Lenses
    criEngineVersion,
    criPubliclyAccessible,
    criAutoMinorVersionUpgrade,
    criReplicationSubnetGroupIdentifier,
    criPreferredMaintenanceWindow,
    criKMSKeyId,
    criAvailabilityZone,
    criVPCSecurityGroupIds,
    criMultiAZ,
    criAllocatedStorage,
    criDNSNameServers,
    criResourceIdentifier,
    criTags,
    criReplicationInstanceIdentifier,
    criReplicationInstanceClass,

    -- * Destructuring the Response
    createReplicationInstanceResponse,
    CreateReplicationInstanceResponse,

    -- * Response Lenses
    crirsReplicationInstance,
    crirsResponseStatus,
  )
where

import Network.AWS.DMS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'createReplicationInstance' smart constructor.
data CreateReplicationInstance = CreateReplicationInstance'
  { _criEngineVersion ::
      !(Maybe Text),
    _criPubliclyAccessible :: !(Maybe Bool),
    _criAutoMinorVersionUpgrade ::
      !(Maybe Bool),
    _criReplicationSubnetGroupIdentifier ::
      !(Maybe Text),
    _criPreferredMaintenanceWindow ::
      !(Maybe Text),
    _criKMSKeyId :: !(Maybe Text),
    _criAvailabilityZone :: !(Maybe Text),
    _criVPCSecurityGroupIds ::
      !(Maybe [Text]),
    _criMultiAZ :: !(Maybe Bool),
    _criAllocatedStorage :: !(Maybe Int),
    _criDNSNameServers :: !(Maybe Text),
    _criResourceIdentifier :: !(Maybe Text),
    _criTags :: !(Maybe [Tag]),
    _criReplicationInstanceIdentifier ::
      !Text,
    _criReplicationInstanceClass :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateReplicationInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'criEngineVersion' - The engine version number of the replication instance. If an engine version number is not specified when a replication instance is created, the default is the latest engine version available.
--
-- * 'criPubliclyAccessible' - Specifies the accessibility options for the replication instance. A value of @true@ represents an instance with a public IP address. A value of @false@ represents an instance with a private IP address. The default value is @true@ .
--
-- * 'criAutoMinorVersionUpgrade' - A value that indicates whether minor engine upgrades are applied automatically to the replication instance during the maintenance window. This parameter defaults to @true@ . Default: @true@
--
-- * 'criReplicationSubnetGroupIdentifier' - A subnet group to associate with the replication instance.
--
-- * 'criPreferredMaintenanceWindow' - The weekly time range during which system maintenance can occur, in Universal Coordinated Time (UTC). Format: @ddd:hh24:mi-ddd:hh24:mi@  Default: A 30-minute window selected at random from an 8-hour block of time per AWS Region, occurring on a random day of the week. Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun Constraints: Minimum 30-minute window.
--
-- * 'criKMSKeyId' - An AWS KMS key identifier that is used to encrypt the data on the replication instance. If you don't specify a value for the @KmsKeyId@ parameter, then AWS DMS uses your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
--
-- * 'criAvailabilityZone' - The Availability Zone where the replication instance will be created. The default value is a random, system-chosen Availability Zone in the endpoint's AWS Region, for example: @us-east-1d@
--
-- * 'criVPCSecurityGroupIds' - Specifies the VPC security group to be used with the replication instance. The VPC security group must work with the VPC containing the replication instance.
--
-- * 'criMultiAZ' - Specifies whether the replication instance is a Multi-AZ deployment. You can't set the @AvailabilityZone@ parameter if the Multi-AZ parameter is set to @true@ .
--
-- * 'criAllocatedStorage' - The amount of storage (in gigabytes) to be initially allocated for the replication instance.
--
-- * 'criDNSNameServers' - A list of custom DNS name servers supported for the replication instance to access your on-premise source or target database. This list overrides the default name servers supported by the replication instance. You can specify a comma-separated list of internet addresses for up to four on-premise DNS name servers. For example: @"1.1.1.1,2.2.2.2,3.3.3.3,4.4.4.4"@
--
-- * 'criResourceIdentifier' - A friendly name for the resource identifier at the end of the @EndpointArn@ response parameter that is returned in the created @Endpoint@ object. The value for this parameter can have up to 31 characters. It can contain only ASCII letters, digits, and hyphen ('-'). Also, it can't end with a hyphen or contain two consecutive hyphens, and can only begin with a letter, such as @Example-App-ARN1@ . For example, this value might result in the @EndpointArn@ value @arn:aws:dms:eu-west-1:012345678901:rep:Example-App-ARN1@ . If you don't specify a @ResourceIdentifier@ value, AWS DMS generates a default identifier value for the end of @EndpointArn@ .
--
-- * 'criTags' - One or more tags to be assigned to the replication instance.
--
-- * 'criReplicationInstanceIdentifier' - The replication instance identifier. This parameter is stored as a lowercase string. Constraints:     * Must contain 1-63 alphanumeric characters or hyphens.     * First character must be a letter.     * Can't end with a hyphen or contain two consecutive hyphens. Example: @myrepinstance@
--
-- * 'criReplicationInstanceClass' - The compute and memory capacity of the replication instance as defined for the specified replication instance class. For example to specify the instance class dms.c4.large, set this parameter to @"dms.c4.large"@ . For more information on the settings and capacities for the available replication instance classes, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_ReplicationInstance.html#CHAP_ReplicationInstance.InDepth Selecting the right AWS DMS replication instance for your migration> .
createReplicationInstance ::
  -- | 'criReplicationInstanceIdentifier'
  Text ->
  -- | 'criReplicationInstanceClass'
  Text ->
  CreateReplicationInstance
createReplicationInstance
  pReplicationInstanceIdentifier_
  pReplicationInstanceClass_ =
    CreateReplicationInstance'
      { _criEngineVersion = Nothing,
        _criPubliclyAccessible = Nothing,
        _criAutoMinorVersionUpgrade = Nothing,
        _criReplicationSubnetGroupIdentifier = Nothing,
        _criPreferredMaintenanceWindow = Nothing,
        _criKMSKeyId = Nothing,
        _criAvailabilityZone = Nothing,
        _criVPCSecurityGroupIds = Nothing,
        _criMultiAZ = Nothing,
        _criAllocatedStorage = Nothing,
        _criDNSNameServers = Nothing,
        _criResourceIdentifier = Nothing,
        _criTags = Nothing,
        _criReplicationInstanceIdentifier =
          pReplicationInstanceIdentifier_,
        _criReplicationInstanceClass = pReplicationInstanceClass_
      }

-- | The engine version number of the replication instance. If an engine version number is not specified when a replication instance is created, the default is the latest engine version available.
criEngineVersion :: Lens' CreateReplicationInstance (Maybe Text)
criEngineVersion = lens _criEngineVersion (\s a -> s {_criEngineVersion = a})

-- | Specifies the accessibility options for the replication instance. A value of @true@ represents an instance with a public IP address. A value of @false@ represents an instance with a private IP address. The default value is @true@ .
criPubliclyAccessible :: Lens' CreateReplicationInstance (Maybe Bool)
criPubliclyAccessible = lens _criPubliclyAccessible (\s a -> s {_criPubliclyAccessible = a})

-- | A value that indicates whether minor engine upgrades are applied automatically to the replication instance during the maintenance window. This parameter defaults to @true@ . Default: @true@
criAutoMinorVersionUpgrade :: Lens' CreateReplicationInstance (Maybe Bool)
criAutoMinorVersionUpgrade = lens _criAutoMinorVersionUpgrade (\s a -> s {_criAutoMinorVersionUpgrade = a})

-- | A subnet group to associate with the replication instance.
criReplicationSubnetGroupIdentifier :: Lens' CreateReplicationInstance (Maybe Text)
criReplicationSubnetGroupIdentifier = lens _criReplicationSubnetGroupIdentifier (\s a -> s {_criReplicationSubnetGroupIdentifier = a})

-- | The weekly time range during which system maintenance can occur, in Universal Coordinated Time (UTC). Format: @ddd:hh24:mi-ddd:hh24:mi@  Default: A 30-minute window selected at random from an 8-hour block of time per AWS Region, occurring on a random day of the week. Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun Constraints: Minimum 30-minute window.
criPreferredMaintenanceWindow :: Lens' CreateReplicationInstance (Maybe Text)
criPreferredMaintenanceWindow = lens _criPreferredMaintenanceWindow (\s a -> s {_criPreferredMaintenanceWindow = a})

-- | An AWS KMS key identifier that is used to encrypt the data on the replication instance. If you don't specify a value for the @KmsKeyId@ parameter, then AWS DMS uses your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
criKMSKeyId :: Lens' CreateReplicationInstance (Maybe Text)
criKMSKeyId = lens _criKMSKeyId (\s a -> s {_criKMSKeyId = a})

-- | The Availability Zone where the replication instance will be created. The default value is a random, system-chosen Availability Zone in the endpoint's AWS Region, for example: @us-east-1d@
criAvailabilityZone :: Lens' CreateReplicationInstance (Maybe Text)
criAvailabilityZone = lens _criAvailabilityZone (\s a -> s {_criAvailabilityZone = a})

-- | Specifies the VPC security group to be used with the replication instance. The VPC security group must work with the VPC containing the replication instance.
criVPCSecurityGroupIds :: Lens' CreateReplicationInstance [Text]
criVPCSecurityGroupIds = lens _criVPCSecurityGroupIds (\s a -> s {_criVPCSecurityGroupIds = a}) . _Default . _Coerce

-- | Specifies whether the replication instance is a Multi-AZ deployment. You can't set the @AvailabilityZone@ parameter if the Multi-AZ parameter is set to @true@ .
criMultiAZ :: Lens' CreateReplicationInstance (Maybe Bool)
criMultiAZ = lens _criMultiAZ (\s a -> s {_criMultiAZ = a})

-- | The amount of storage (in gigabytes) to be initially allocated for the replication instance.
criAllocatedStorage :: Lens' CreateReplicationInstance (Maybe Int)
criAllocatedStorage = lens _criAllocatedStorage (\s a -> s {_criAllocatedStorage = a})

-- | A list of custom DNS name servers supported for the replication instance to access your on-premise source or target database. This list overrides the default name servers supported by the replication instance. You can specify a comma-separated list of internet addresses for up to four on-premise DNS name servers. For example: @"1.1.1.1,2.2.2.2,3.3.3.3,4.4.4.4"@
criDNSNameServers :: Lens' CreateReplicationInstance (Maybe Text)
criDNSNameServers = lens _criDNSNameServers (\s a -> s {_criDNSNameServers = a})

-- | A friendly name for the resource identifier at the end of the @EndpointArn@ response parameter that is returned in the created @Endpoint@ object. The value for this parameter can have up to 31 characters. It can contain only ASCII letters, digits, and hyphen ('-'). Also, it can't end with a hyphen or contain two consecutive hyphens, and can only begin with a letter, such as @Example-App-ARN1@ . For example, this value might result in the @EndpointArn@ value @arn:aws:dms:eu-west-1:012345678901:rep:Example-App-ARN1@ . If you don't specify a @ResourceIdentifier@ value, AWS DMS generates a default identifier value for the end of @EndpointArn@ .
criResourceIdentifier :: Lens' CreateReplicationInstance (Maybe Text)
criResourceIdentifier = lens _criResourceIdentifier (\s a -> s {_criResourceIdentifier = a})

-- | One or more tags to be assigned to the replication instance.
criTags :: Lens' CreateReplicationInstance [Tag]
criTags = lens _criTags (\s a -> s {_criTags = a}) . _Default . _Coerce

-- | The replication instance identifier. This parameter is stored as a lowercase string. Constraints:     * Must contain 1-63 alphanumeric characters or hyphens.     * First character must be a letter.     * Can't end with a hyphen or contain two consecutive hyphens. Example: @myrepinstance@
criReplicationInstanceIdentifier :: Lens' CreateReplicationInstance Text
criReplicationInstanceIdentifier = lens _criReplicationInstanceIdentifier (\s a -> s {_criReplicationInstanceIdentifier = a})

-- | The compute and memory capacity of the replication instance as defined for the specified replication instance class. For example to specify the instance class dms.c4.large, set this parameter to @"dms.c4.large"@ . For more information on the settings and capacities for the available replication instance classes, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_ReplicationInstance.html#CHAP_ReplicationInstance.InDepth Selecting the right AWS DMS replication instance for your migration> .
criReplicationInstanceClass :: Lens' CreateReplicationInstance Text
criReplicationInstanceClass = lens _criReplicationInstanceClass (\s a -> s {_criReplicationInstanceClass = a})

instance AWSRequest CreateReplicationInstance where
  type
    Rs CreateReplicationInstance =
      CreateReplicationInstanceResponse
  request = postJSON dms
  response =
    receiveJSON
      ( \s h x ->
          CreateReplicationInstanceResponse'
            <$> (x .?> "ReplicationInstance") <*> (pure (fromEnum s))
      )

instance Hashable CreateReplicationInstance

instance NFData CreateReplicationInstance

instance ToHeaders CreateReplicationInstance where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AmazonDMSv20160101.CreateReplicationInstance" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateReplicationInstance where
  toJSON CreateReplicationInstance' {..} =
    object
      ( catMaybes
          [ ("EngineVersion" .=) <$> _criEngineVersion,
            ("PubliclyAccessible" .=) <$> _criPubliclyAccessible,
            ("AutoMinorVersionUpgrade" .=) <$> _criAutoMinorVersionUpgrade,
            ("ReplicationSubnetGroupIdentifier" .=)
              <$> _criReplicationSubnetGroupIdentifier,
            ("PreferredMaintenanceWindow" .=)
              <$> _criPreferredMaintenanceWindow,
            ("KmsKeyId" .=) <$> _criKMSKeyId,
            ("AvailabilityZone" .=) <$> _criAvailabilityZone,
            ("VpcSecurityGroupIds" .=) <$> _criVPCSecurityGroupIds,
            ("MultiAZ" .=) <$> _criMultiAZ,
            ("AllocatedStorage" .=) <$> _criAllocatedStorage,
            ("DnsNameServers" .=) <$> _criDNSNameServers,
            ("ResourceIdentifier" .=) <$> _criResourceIdentifier,
            ("Tags" .=) <$> _criTags,
            Just
              ( "ReplicationInstanceIdentifier"
                  .= _criReplicationInstanceIdentifier
              ),
            Just ("ReplicationInstanceClass" .= _criReplicationInstanceClass)
          ]
      )

instance ToPath CreateReplicationInstance where
  toPath = const "/"

instance ToQuery CreateReplicationInstance where
  toQuery = const mempty

-- |
--
--
--
-- /See:/ 'createReplicationInstanceResponse' smart constructor.
data CreateReplicationInstanceResponse = CreateReplicationInstanceResponse'
  { _crirsReplicationInstance ::
      !( Maybe
           ReplicationInstance
       ),
    _crirsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateReplicationInstanceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crirsReplicationInstance' - The replication instance that was created.
--
-- * 'crirsResponseStatus' - -- | The response status code.
createReplicationInstanceResponse ::
  -- | 'crirsResponseStatus'
  Int ->
  CreateReplicationInstanceResponse
createReplicationInstanceResponse pResponseStatus_ =
  CreateReplicationInstanceResponse'
    { _crirsReplicationInstance =
        Nothing,
      _crirsResponseStatus = pResponseStatus_
    }

-- | The replication instance that was created.
crirsReplicationInstance :: Lens' CreateReplicationInstanceResponse (Maybe ReplicationInstance)
crirsReplicationInstance = lens _crirsReplicationInstance (\s a -> s {_crirsReplicationInstance = a})

-- | -- | The response status code.
crirsResponseStatus :: Lens' CreateReplicationInstanceResponse Int
crirsResponseStatus = lens _crirsResponseStatus (\s a -> s {_crirsResponseStatus = a})

instance NFData CreateReplicationInstanceResponse
