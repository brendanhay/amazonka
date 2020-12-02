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
-- Module      : Network.AWS.DMS.CreateReplicationInstance
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates the replication instance using the specified parameters.
--
--
module Network.AWS.DMS.CreateReplicationInstance
    (
    -- * Creating a Request
      createReplicationInstance
    , CreateReplicationInstance
    -- * Request Lenses
    , criEngineVersion
    , criPubliclyAccessible
    , criAutoMinorVersionUpgrade
    , criReplicationSubnetGroupIdentifier
    , criPreferredMaintenanceWindow
    , criKMSKeyId
    , criAvailabilityZone
    , criVPCSecurityGroupIds
    , criMultiAZ
    , criAllocatedStorage
    , criTags
    , criReplicationInstanceIdentifier
    , criReplicationInstanceClass

    -- * Destructuring the Response
    , createReplicationInstanceResponse
    , CreateReplicationInstanceResponse
    -- * Response Lenses
    , crirsReplicationInstance
    , crirsResponseStatus
    ) where

import Network.AWS.DMS.Types
import Network.AWS.DMS.Types.Product
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
  { _criEngineVersion                    :: !(Maybe Text)
  , _criPubliclyAccessible               :: !(Maybe Bool)
  , _criAutoMinorVersionUpgrade          :: !(Maybe Bool)
  , _criReplicationSubnetGroupIdentifier :: !(Maybe Text)
  , _criPreferredMaintenanceWindow       :: !(Maybe Text)
  , _criKMSKeyId                         :: !(Maybe Text)
  , _criAvailabilityZone                 :: !(Maybe Text)
  , _criVPCSecurityGroupIds              :: !(Maybe [Text])
  , _criMultiAZ                          :: !(Maybe Bool)
  , _criAllocatedStorage                 :: !(Maybe Int)
  , _criTags                             :: !(Maybe [Tag])
  , _criReplicationInstanceIdentifier    :: !Text
  , _criReplicationInstanceClass         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateReplicationInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'criEngineVersion' - The engine version number of the replication instance.
--
-- * 'criPubliclyAccessible' - Specifies the accessibility options for the replication instance. A value of @true@ represents an instance with a public IP address. A value of @false@ represents an instance with a private IP address. The default value is @true@ .
--
-- * 'criAutoMinorVersionUpgrade' - Indicates that minor engine upgrades will be applied automatically to the replication instance during the maintenance window. Default: @true@
--
-- * 'criReplicationSubnetGroupIdentifier' - A subnet group to associate with the replication instance.
--
-- * 'criPreferredMaintenanceWindow' - The weekly time range during which system maintenance can occur, in Universal Coordinated Time (UTC). Format: @ddd:hh24:mi-ddd:hh24:mi@  Default: A 30-minute window selected at random from an 8-hour block of time per region, occurring on a random day of the week. Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun Constraints: Minimum 30-minute window.
--
-- * 'criKMSKeyId' - The KMS key identifier that will be used to encrypt the content on the replication instance. If you do not specify a value for the KmsKeyId parameter, then AWS DMS will use your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS region.
--
-- * 'criAvailabilityZone' - The EC2 Availability Zone that the replication instance will be created in. Default: A random, system-chosen Availability Zone in the endpoint's region. Example: @us-east-1d@
--
-- * 'criVPCSecurityGroupIds' - Specifies the VPC security group to be used with the replication instance. The VPC security group must work with the VPC containing the replication instance.
--
-- * 'criMultiAZ' - Specifies if the replication instance is a Multi-AZ deployment. You cannot set the @AvailabilityZone@ parameter if the Multi-AZ parameter is set to @true@ .
--
-- * 'criAllocatedStorage' - The amount of storage (in gigabytes) to be initially allocated for the replication instance.
--
-- * 'criTags' - Tags to be associated with the replication instance.
--
-- * 'criReplicationInstanceIdentifier' - The replication instance identifier. This parameter is stored as a lowercase string. Constraints:     * Must contain from 1 to 63 alphanumeric characters or hyphens.     * First character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens. Example: @myrepinstance@
--
-- * 'criReplicationInstanceClass' - The compute and memory capacity of the replication instance as specified by the replication instance class. Valid Values: @dms.t2.micro | dms.t2.small | dms.t2.medium | dms.t2.large | dms.c4.large | dms.c4.xlarge | dms.c4.2xlarge | dms.c4.4xlarge @
createReplicationInstance
    :: Text -- ^ 'criReplicationInstanceIdentifier'
    -> Text -- ^ 'criReplicationInstanceClass'
    -> CreateReplicationInstance
createReplicationInstance pReplicationInstanceIdentifier_ pReplicationInstanceClass_ =
  CreateReplicationInstance'
    { _criEngineVersion = Nothing
    , _criPubliclyAccessible = Nothing
    , _criAutoMinorVersionUpgrade = Nothing
    , _criReplicationSubnetGroupIdentifier = Nothing
    , _criPreferredMaintenanceWindow = Nothing
    , _criKMSKeyId = Nothing
    , _criAvailabilityZone = Nothing
    , _criVPCSecurityGroupIds = Nothing
    , _criMultiAZ = Nothing
    , _criAllocatedStorage = Nothing
    , _criTags = Nothing
    , _criReplicationInstanceIdentifier = pReplicationInstanceIdentifier_
    , _criReplicationInstanceClass = pReplicationInstanceClass_
    }


-- | The engine version number of the replication instance.
criEngineVersion :: Lens' CreateReplicationInstance (Maybe Text)
criEngineVersion = lens _criEngineVersion (\ s a -> s{_criEngineVersion = a})

-- | Specifies the accessibility options for the replication instance. A value of @true@ represents an instance with a public IP address. A value of @false@ represents an instance with a private IP address. The default value is @true@ .
criPubliclyAccessible :: Lens' CreateReplicationInstance (Maybe Bool)
criPubliclyAccessible = lens _criPubliclyAccessible (\ s a -> s{_criPubliclyAccessible = a})

-- | Indicates that minor engine upgrades will be applied automatically to the replication instance during the maintenance window. Default: @true@
criAutoMinorVersionUpgrade :: Lens' CreateReplicationInstance (Maybe Bool)
criAutoMinorVersionUpgrade = lens _criAutoMinorVersionUpgrade (\ s a -> s{_criAutoMinorVersionUpgrade = a})

-- | A subnet group to associate with the replication instance.
criReplicationSubnetGroupIdentifier :: Lens' CreateReplicationInstance (Maybe Text)
criReplicationSubnetGroupIdentifier = lens _criReplicationSubnetGroupIdentifier (\ s a -> s{_criReplicationSubnetGroupIdentifier = a})

-- | The weekly time range during which system maintenance can occur, in Universal Coordinated Time (UTC). Format: @ddd:hh24:mi-ddd:hh24:mi@  Default: A 30-minute window selected at random from an 8-hour block of time per region, occurring on a random day of the week. Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun Constraints: Minimum 30-minute window.
criPreferredMaintenanceWindow :: Lens' CreateReplicationInstance (Maybe Text)
criPreferredMaintenanceWindow = lens _criPreferredMaintenanceWindow (\ s a -> s{_criPreferredMaintenanceWindow = a})

-- | The KMS key identifier that will be used to encrypt the content on the replication instance. If you do not specify a value for the KmsKeyId parameter, then AWS DMS will use your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS region.
criKMSKeyId :: Lens' CreateReplicationInstance (Maybe Text)
criKMSKeyId = lens _criKMSKeyId (\ s a -> s{_criKMSKeyId = a})

-- | The EC2 Availability Zone that the replication instance will be created in. Default: A random, system-chosen Availability Zone in the endpoint's region. Example: @us-east-1d@
criAvailabilityZone :: Lens' CreateReplicationInstance (Maybe Text)
criAvailabilityZone = lens _criAvailabilityZone (\ s a -> s{_criAvailabilityZone = a})

-- | Specifies the VPC security group to be used with the replication instance. The VPC security group must work with the VPC containing the replication instance.
criVPCSecurityGroupIds :: Lens' CreateReplicationInstance [Text]
criVPCSecurityGroupIds = lens _criVPCSecurityGroupIds (\ s a -> s{_criVPCSecurityGroupIds = a}) . _Default . _Coerce

-- | Specifies if the replication instance is a Multi-AZ deployment. You cannot set the @AvailabilityZone@ parameter if the Multi-AZ parameter is set to @true@ .
criMultiAZ :: Lens' CreateReplicationInstance (Maybe Bool)
criMultiAZ = lens _criMultiAZ (\ s a -> s{_criMultiAZ = a})

-- | The amount of storage (in gigabytes) to be initially allocated for the replication instance.
criAllocatedStorage :: Lens' CreateReplicationInstance (Maybe Int)
criAllocatedStorage = lens _criAllocatedStorage (\ s a -> s{_criAllocatedStorage = a})

-- | Tags to be associated with the replication instance.
criTags :: Lens' CreateReplicationInstance [Tag]
criTags = lens _criTags (\ s a -> s{_criTags = a}) . _Default . _Coerce

-- | The replication instance identifier. This parameter is stored as a lowercase string. Constraints:     * Must contain from 1 to 63 alphanumeric characters or hyphens.     * First character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens. Example: @myrepinstance@
criReplicationInstanceIdentifier :: Lens' CreateReplicationInstance Text
criReplicationInstanceIdentifier = lens _criReplicationInstanceIdentifier (\ s a -> s{_criReplicationInstanceIdentifier = a})

-- | The compute and memory capacity of the replication instance as specified by the replication instance class. Valid Values: @dms.t2.micro | dms.t2.small | dms.t2.medium | dms.t2.large | dms.c4.large | dms.c4.xlarge | dms.c4.2xlarge | dms.c4.4xlarge @
criReplicationInstanceClass :: Lens' CreateReplicationInstance Text
criReplicationInstanceClass = lens _criReplicationInstanceClass (\ s a -> s{_criReplicationInstanceClass = a})

instance AWSRequest CreateReplicationInstance where
        type Rs CreateReplicationInstance =
             CreateReplicationInstanceResponse
        request = postJSON dms
        response
          = receiveJSON
              (\ s h x ->
                 CreateReplicationInstanceResponse' <$>
                   (x .?> "ReplicationInstance") <*>
                     (pure (fromEnum s)))

instance Hashable CreateReplicationInstance where

instance NFData CreateReplicationInstance where

instance ToHeaders CreateReplicationInstance where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonDMSv20160101.CreateReplicationInstance" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateReplicationInstance where
        toJSON CreateReplicationInstance'{..}
          = object
              (catMaybes
                 [("EngineVersion" .=) <$> _criEngineVersion,
                  ("PubliclyAccessible" .=) <$> _criPubliclyAccessible,
                  ("AutoMinorVersionUpgrade" .=) <$>
                    _criAutoMinorVersionUpgrade,
                  ("ReplicationSubnetGroupIdentifier" .=) <$>
                    _criReplicationSubnetGroupIdentifier,
                  ("PreferredMaintenanceWindow" .=) <$>
                    _criPreferredMaintenanceWindow,
                  ("KmsKeyId" .=) <$> _criKMSKeyId,
                  ("AvailabilityZone" .=) <$> _criAvailabilityZone,
                  ("VpcSecurityGroupIds" .=) <$>
                    _criVPCSecurityGroupIds,
                  ("MultiAZ" .=) <$> _criMultiAZ,
                  ("AllocatedStorage" .=) <$> _criAllocatedStorage,
                  ("Tags" .=) <$> _criTags,
                  Just
                    ("ReplicationInstanceIdentifier" .=
                       _criReplicationInstanceIdentifier),
                  Just
                    ("ReplicationInstanceClass" .=
                       _criReplicationInstanceClass)])

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
  { _crirsReplicationInstance :: !(Maybe ReplicationInstance)
  , _crirsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateReplicationInstanceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crirsReplicationInstance' - The replication instance that was created.
--
-- * 'crirsResponseStatus' - -- | The response status code.
createReplicationInstanceResponse
    :: Int -- ^ 'crirsResponseStatus'
    -> CreateReplicationInstanceResponse
createReplicationInstanceResponse pResponseStatus_ =
  CreateReplicationInstanceResponse'
    { _crirsReplicationInstance = Nothing
    , _crirsResponseStatus = pResponseStatus_
    }


-- | The replication instance that was created.
crirsReplicationInstance :: Lens' CreateReplicationInstanceResponse (Maybe ReplicationInstance)
crirsReplicationInstance = lens _crirsReplicationInstance (\ s a -> s{_crirsReplicationInstance = a})

-- | -- | The response status code.
crirsResponseStatus :: Lens' CreateReplicationInstanceResponse Int
crirsResponseStatus = lens _crirsResponseStatus (\ s a -> s{_crirsResponseStatus = a})

instance NFData CreateReplicationInstanceResponse
         where
