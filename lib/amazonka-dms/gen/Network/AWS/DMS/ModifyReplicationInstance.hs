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
-- Module      : Network.AWS.DMS.ModifyReplicationInstance
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the replication instance to apply new settings. You can change one or more parameters by specifying these parameters and the new values in the request.
--
--
-- Some settings are applied during the maintenance window.
--
--
--
module Network.AWS.DMS.ModifyReplicationInstance
    (
    -- * Creating a Request
      modifyReplicationInstance
    , ModifyReplicationInstance
    -- * Request Lenses
    , mriEngineVersion
    , mriAutoMinorVersionUpgrade
    , mriAllowMajorVersionUpgrade
    , mriPreferredMaintenanceWindow
    , mriVPCSecurityGroupIds
    , mriMultiAZ
    , mriAllocatedStorage
    , mriApplyImmediately
    , mriReplicationInstanceClass
    , mriReplicationInstanceIdentifier
    , mriReplicationInstanceARN

    -- * Destructuring the Response
    , modifyReplicationInstanceResponse
    , ModifyReplicationInstanceResponse
    -- * Response Lenses
    , mrirsReplicationInstance
    , mrirsResponseStatus
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
-- /See:/ 'modifyReplicationInstance' smart constructor.
data ModifyReplicationInstance = ModifyReplicationInstance'
  { _mriEngineVersion                 :: !(Maybe Text)
  , _mriAutoMinorVersionUpgrade       :: !(Maybe Bool)
  , _mriAllowMajorVersionUpgrade      :: !(Maybe Bool)
  , _mriPreferredMaintenanceWindow    :: !(Maybe Text)
  , _mriVPCSecurityGroupIds           :: !(Maybe [Text])
  , _mriMultiAZ                       :: !(Maybe Bool)
  , _mriAllocatedStorage              :: !(Maybe Int)
  , _mriApplyImmediately              :: !(Maybe Bool)
  , _mriReplicationInstanceClass      :: !(Maybe Text)
  , _mriReplicationInstanceIdentifier :: !(Maybe Text)
  , _mriReplicationInstanceARN        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyReplicationInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mriEngineVersion' - The engine version number of the replication instance.
--
-- * 'mriAutoMinorVersionUpgrade' - Indicates that minor version upgrades will be applied automatically to the replication instance during the maintenance window. Changing this parameter does not result in an outage except in the following case and the change is asynchronously applied as soon as possible. An outage will result if this parameter is set to @true@ during the maintenance window, and a newer minor version is available, and AWS DMS has enabled auto patching for that engine version.
--
-- * 'mriAllowMajorVersionUpgrade' - Indicates that major version upgrades are allowed. Changing this parameter does not result in an outage and the change is asynchronously applied as soon as possible. Constraints: This parameter must be set to true when specifying a value for the @EngineVersion@ parameter that is a different major version than the replication instance's current version.
--
-- * 'mriPreferredMaintenanceWindow' - The weekly time range (in UTC) during which system maintenance can occur, which might result in an outage. Changing this parameter does not result in an outage, except in the following situation, and the change is asynchronously applied as soon as possible. If moving this window to the current time, there must be at least 30 minutes between the current time and end of the window to ensure pending changes are applied. Default: Uses existing setting Format: ddd:hh24:mi-ddd:hh24:mi Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun Constraints: Must be at least 30 minutes
--
-- * 'mriVPCSecurityGroupIds' - Specifies the VPC security group to be used with the replication instance. The VPC security group must work with the VPC containing the replication instance.
--
-- * 'mriMultiAZ' - Specifies if the replication instance is a Multi-AZ deployment. You cannot set the @AvailabilityZone@ parameter if the Multi-AZ parameter is set to @true@ .
--
-- * 'mriAllocatedStorage' - The amount of storage (in gigabytes) to be allocated for the replication instance.
--
-- * 'mriApplyImmediately' - Indicates whether the changes should be applied immediately or during the next maintenance window.
--
-- * 'mriReplicationInstanceClass' - The compute and memory capacity of the replication instance. Valid Values: @dms.t2.micro | dms.t2.small | dms.t2.medium | dms.t2.large | dms.c4.large | dms.c4.xlarge | dms.c4.2xlarge | dms.c4.4xlarge @
--
-- * 'mriReplicationInstanceIdentifier' - The replication instance identifier. This parameter is stored as a lowercase string.
--
-- * 'mriReplicationInstanceARN' - The Amazon Resource Name (ARN) of the replication instance.
modifyReplicationInstance
    :: Text -- ^ 'mriReplicationInstanceARN'
    -> ModifyReplicationInstance
modifyReplicationInstance pReplicationInstanceARN_ =
  ModifyReplicationInstance'
    { _mriEngineVersion = Nothing
    , _mriAutoMinorVersionUpgrade = Nothing
    , _mriAllowMajorVersionUpgrade = Nothing
    , _mriPreferredMaintenanceWindow = Nothing
    , _mriVPCSecurityGroupIds = Nothing
    , _mriMultiAZ = Nothing
    , _mriAllocatedStorage = Nothing
    , _mriApplyImmediately = Nothing
    , _mriReplicationInstanceClass = Nothing
    , _mriReplicationInstanceIdentifier = Nothing
    , _mriReplicationInstanceARN = pReplicationInstanceARN_
    }


-- | The engine version number of the replication instance.
mriEngineVersion :: Lens' ModifyReplicationInstance (Maybe Text)
mriEngineVersion = lens _mriEngineVersion (\ s a -> s{_mriEngineVersion = a})

-- | Indicates that minor version upgrades will be applied automatically to the replication instance during the maintenance window. Changing this parameter does not result in an outage except in the following case and the change is asynchronously applied as soon as possible. An outage will result if this parameter is set to @true@ during the maintenance window, and a newer minor version is available, and AWS DMS has enabled auto patching for that engine version.
mriAutoMinorVersionUpgrade :: Lens' ModifyReplicationInstance (Maybe Bool)
mriAutoMinorVersionUpgrade = lens _mriAutoMinorVersionUpgrade (\ s a -> s{_mriAutoMinorVersionUpgrade = a})

-- | Indicates that major version upgrades are allowed. Changing this parameter does not result in an outage and the change is asynchronously applied as soon as possible. Constraints: This parameter must be set to true when specifying a value for the @EngineVersion@ parameter that is a different major version than the replication instance's current version.
mriAllowMajorVersionUpgrade :: Lens' ModifyReplicationInstance (Maybe Bool)
mriAllowMajorVersionUpgrade = lens _mriAllowMajorVersionUpgrade (\ s a -> s{_mriAllowMajorVersionUpgrade = a})

-- | The weekly time range (in UTC) during which system maintenance can occur, which might result in an outage. Changing this parameter does not result in an outage, except in the following situation, and the change is asynchronously applied as soon as possible. If moving this window to the current time, there must be at least 30 minutes between the current time and end of the window to ensure pending changes are applied. Default: Uses existing setting Format: ddd:hh24:mi-ddd:hh24:mi Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun Constraints: Must be at least 30 minutes
mriPreferredMaintenanceWindow :: Lens' ModifyReplicationInstance (Maybe Text)
mriPreferredMaintenanceWindow = lens _mriPreferredMaintenanceWindow (\ s a -> s{_mriPreferredMaintenanceWindow = a})

-- | Specifies the VPC security group to be used with the replication instance. The VPC security group must work with the VPC containing the replication instance.
mriVPCSecurityGroupIds :: Lens' ModifyReplicationInstance [Text]
mriVPCSecurityGroupIds = lens _mriVPCSecurityGroupIds (\ s a -> s{_mriVPCSecurityGroupIds = a}) . _Default . _Coerce

-- | Specifies if the replication instance is a Multi-AZ deployment. You cannot set the @AvailabilityZone@ parameter if the Multi-AZ parameter is set to @true@ .
mriMultiAZ :: Lens' ModifyReplicationInstance (Maybe Bool)
mriMultiAZ = lens _mriMultiAZ (\ s a -> s{_mriMultiAZ = a})

-- | The amount of storage (in gigabytes) to be allocated for the replication instance.
mriAllocatedStorage :: Lens' ModifyReplicationInstance (Maybe Int)
mriAllocatedStorage = lens _mriAllocatedStorage (\ s a -> s{_mriAllocatedStorage = a})

-- | Indicates whether the changes should be applied immediately or during the next maintenance window.
mriApplyImmediately :: Lens' ModifyReplicationInstance (Maybe Bool)
mriApplyImmediately = lens _mriApplyImmediately (\ s a -> s{_mriApplyImmediately = a})

-- | The compute and memory capacity of the replication instance. Valid Values: @dms.t2.micro | dms.t2.small | dms.t2.medium | dms.t2.large | dms.c4.large | dms.c4.xlarge | dms.c4.2xlarge | dms.c4.4xlarge @
mriReplicationInstanceClass :: Lens' ModifyReplicationInstance (Maybe Text)
mriReplicationInstanceClass = lens _mriReplicationInstanceClass (\ s a -> s{_mriReplicationInstanceClass = a})

-- | The replication instance identifier. This parameter is stored as a lowercase string.
mriReplicationInstanceIdentifier :: Lens' ModifyReplicationInstance (Maybe Text)
mriReplicationInstanceIdentifier = lens _mriReplicationInstanceIdentifier (\ s a -> s{_mriReplicationInstanceIdentifier = a})

-- | The Amazon Resource Name (ARN) of the replication instance.
mriReplicationInstanceARN :: Lens' ModifyReplicationInstance Text
mriReplicationInstanceARN = lens _mriReplicationInstanceARN (\ s a -> s{_mriReplicationInstanceARN = a})

instance AWSRequest ModifyReplicationInstance where
        type Rs ModifyReplicationInstance =
             ModifyReplicationInstanceResponse
        request = postJSON dms
        response
          = receiveJSON
              (\ s h x ->
                 ModifyReplicationInstanceResponse' <$>
                   (x .?> "ReplicationInstance") <*>
                     (pure (fromEnum s)))

instance Hashable ModifyReplicationInstance where

instance NFData ModifyReplicationInstance where

instance ToHeaders ModifyReplicationInstance where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonDMSv20160101.ModifyReplicationInstance" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ModifyReplicationInstance where
        toJSON ModifyReplicationInstance'{..}
          = object
              (catMaybes
                 [("EngineVersion" .=) <$> _mriEngineVersion,
                  ("AutoMinorVersionUpgrade" .=) <$>
                    _mriAutoMinorVersionUpgrade,
                  ("AllowMajorVersionUpgrade" .=) <$>
                    _mriAllowMajorVersionUpgrade,
                  ("PreferredMaintenanceWindow" .=) <$>
                    _mriPreferredMaintenanceWindow,
                  ("VpcSecurityGroupIds" .=) <$>
                    _mriVPCSecurityGroupIds,
                  ("MultiAZ" .=) <$> _mriMultiAZ,
                  ("AllocatedStorage" .=) <$> _mriAllocatedStorage,
                  ("ApplyImmediately" .=) <$> _mriApplyImmediately,
                  ("ReplicationInstanceClass" .=) <$>
                    _mriReplicationInstanceClass,
                  ("ReplicationInstanceIdentifier" .=) <$>
                    _mriReplicationInstanceIdentifier,
                  Just
                    ("ReplicationInstanceArn" .=
                       _mriReplicationInstanceARN)])

instance ToPath ModifyReplicationInstance where
        toPath = const "/"

instance ToQuery ModifyReplicationInstance where
        toQuery = const mempty

-- |
--
--
--
-- /See:/ 'modifyReplicationInstanceResponse' smart constructor.
data ModifyReplicationInstanceResponse = ModifyReplicationInstanceResponse'
  { _mrirsReplicationInstance :: !(Maybe ReplicationInstance)
  , _mrirsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyReplicationInstanceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mrirsReplicationInstance' - The modified replication instance.
--
-- * 'mrirsResponseStatus' - -- | The response status code.
modifyReplicationInstanceResponse
    :: Int -- ^ 'mrirsResponseStatus'
    -> ModifyReplicationInstanceResponse
modifyReplicationInstanceResponse pResponseStatus_ =
  ModifyReplicationInstanceResponse'
    { _mrirsReplicationInstance = Nothing
    , _mrirsResponseStatus = pResponseStatus_
    }


-- | The modified replication instance.
mrirsReplicationInstance :: Lens' ModifyReplicationInstanceResponse (Maybe ReplicationInstance)
mrirsReplicationInstance = lens _mrirsReplicationInstance (\ s a -> s{_mrirsReplicationInstance = a})

-- | -- | The response status code.
mrirsResponseStatus :: Lens' ModifyReplicationInstanceResponse Int
mrirsResponseStatus = lens _mrirsResponseStatus (\ s a -> s{_mrirsResponseStatus = a})

instance NFData ModifyReplicationInstanceResponse
         where
