{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.OrderableReplicationInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.OrderableReplicationInstance where

import Network.AWS.DMS.Types.ReleaseStatusValues
import Network.AWS.Lens
import Network.AWS.Prelude

-- | In response to the @DescribeOrderableReplicationInstances@ operation, this object describes an available replication instance. This description includes the replication instance's type, engine version, and allocated storage.
--
--
--
-- /See:/ 'orderableReplicationInstance' smart constructor.
data OrderableReplicationInstance = OrderableReplicationInstance'
  { _oriEngineVersion ::
      !(Maybe Text),
    _oriMinAllocatedStorage ::
      !(Maybe Int),
    _oriReleaseStatus ::
      !(Maybe ReleaseStatusValues),
    _oriIncludedAllocatedStorage ::
      !(Maybe Int),
    _oriAvailabilityZones ::
      !(Maybe [Text]),
    _oriMaxAllocatedStorage ::
      !(Maybe Int),
    _oriReplicationInstanceClass ::
      !(Maybe Text),
    _oriDefaultAllocatedStorage ::
      !(Maybe Int),
    _oriStorageType :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OrderableReplicationInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oriEngineVersion' - The version of the replication engine.
--
-- * 'oriMinAllocatedStorage' - The minimum amount of storage (in gigabytes) that can be allocated for the replication instance.
--
-- * 'oriReleaseStatus' - The value returned when the specified @EngineVersion@ of the replication instance is in Beta or test mode. This indicates some features might not work as expected.
--
-- * 'oriIncludedAllocatedStorage' - The amount of storage (in gigabytes) that is allocated for the replication instance.
--
-- * 'oriAvailabilityZones' - List of Availability Zones for this replication instance.
--
-- * 'oriMaxAllocatedStorage' - The minimum amount of storage (in gigabytes) that can be allocated for the replication instance.
--
-- * 'oriReplicationInstanceClass' - The compute and memory capacity of the replication instance as defined for the specified replication instance class. For example to specify the instance class dms.c4.large, set this parameter to @"dms.c4.large"@ . For more information on the settings and capacities for the available replication instance classes, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_ReplicationInstance.html#CHAP_ReplicationInstance.InDepth Selecting the right AWS DMS replication instance for your migration> .
--
-- * 'oriDefaultAllocatedStorage' - The default amount of storage (in gigabytes) that is allocated for the replication instance.
--
-- * 'oriStorageType' - The type of storage used by the replication instance.
orderableReplicationInstance ::
  OrderableReplicationInstance
orderableReplicationInstance =
  OrderableReplicationInstance'
    { _oriEngineVersion = Nothing,
      _oriMinAllocatedStorage = Nothing,
      _oriReleaseStatus = Nothing,
      _oriIncludedAllocatedStorage = Nothing,
      _oriAvailabilityZones = Nothing,
      _oriMaxAllocatedStorage = Nothing,
      _oriReplicationInstanceClass = Nothing,
      _oriDefaultAllocatedStorage = Nothing,
      _oriStorageType = Nothing
    }

-- | The version of the replication engine.
oriEngineVersion :: Lens' OrderableReplicationInstance (Maybe Text)
oriEngineVersion = lens _oriEngineVersion (\s a -> s {_oriEngineVersion = a})

-- | The minimum amount of storage (in gigabytes) that can be allocated for the replication instance.
oriMinAllocatedStorage :: Lens' OrderableReplicationInstance (Maybe Int)
oriMinAllocatedStorage = lens _oriMinAllocatedStorage (\s a -> s {_oriMinAllocatedStorage = a})

-- | The value returned when the specified @EngineVersion@ of the replication instance is in Beta or test mode. This indicates some features might not work as expected.
oriReleaseStatus :: Lens' OrderableReplicationInstance (Maybe ReleaseStatusValues)
oriReleaseStatus = lens _oriReleaseStatus (\s a -> s {_oriReleaseStatus = a})

-- | The amount of storage (in gigabytes) that is allocated for the replication instance.
oriIncludedAllocatedStorage :: Lens' OrderableReplicationInstance (Maybe Int)
oriIncludedAllocatedStorage = lens _oriIncludedAllocatedStorage (\s a -> s {_oriIncludedAllocatedStorage = a})

-- | List of Availability Zones for this replication instance.
oriAvailabilityZones :: Lens' OrderableReplicationInstance [Text]
oriAvailabilityZones = lens _oriAvailabilityZones (\s a -> s {_oriAvailabilityZones = a}) . _Default . _Coerce

-- | The minimum amount of storage (in gigabytes) that can be allocated for the replication instance.
oriMaxAllocatedStorage :: Lens' OrderableReplicationInstance (Maybe Int)
oriMaxAllocatedStorage = lens _oriMaxAllocatedStorage (\s a -> s {_oriMaxAllocatedStorage = a})

-- | The compute and memory capacity of the replication instance as defined for the specified replication instance class. For example to specify the instance class dms.c4.large, set this parameter to @"dms.c4.large"@ . For more information on the settings and capacities for the available replication instance classes, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_ReplicationInstance.html#CHAP_ReplicationInstance.InDepth Selecting the right AWS DMS replication instance for your migration> .
oriReplicationInstanceClass :: Lens' OrderableReplicationInstance (Maybe Text)
oriReplicationInstanceClass = lens _oriReplicationInstanceClass (\s a -> s {_oriReplicationInstanceClass = a})

-- | The default amount of storage (in gigabytes) that is allocated for the replication instance.
oriDefaultAllocatedStorage :: Lens' OrderableReplicationInstance (Maybe Int)
oriDefaultAllocatedStorage = lens _oriDefaultAllocatedStorage (\s a -> s {_oriDefaultAllocatedStorage = a})

-- | The type of storage used by the replication instance.
oriStorageType :: Lens' OrderableReplicationInstance (Maybe Text)
oriStorageType = lens _oriStorageType (\s a -> s {_oriStorageType = a})

instance FromJSON OrderableReplicationInstance where
  parseJSON =
    withObject
      "OrderableReplicationInstance"
      ( \x ->
          OrderableReplicationInstance'
            <$> (x .:? "EngineVersion")
            <*> (x .:? "MinAllocatedStorage")
            <*> (x .:? "ReleaseStatus")
            <*> (x .:? "IncludedAllocatedStorage")
            <*> (x .:? "AvailabilityZones" .!= mempty)
            <*> (x .:? "MaxAllocatedStorage")
            <*> (x .:? "ReplicationInstanceClass")
            <*> (x .:? "DefaultAllocatedStorage")
            <*> (x .:? "StorageType")
      )

instance Hashable OrderableReplicationInstance

instance NFData OrderableReplicationInstance
