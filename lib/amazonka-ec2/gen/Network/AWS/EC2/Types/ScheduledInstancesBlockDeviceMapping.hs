{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ScheduledInstancesBlockDeviceMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ScheduledInstancesBlockDeviceMapping where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ScheduledInstancesEBS
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a block device mapping for a Scheduled Instance.
--
--
--
-- /See:/ 'scheduledInstancesBlockDeviceMapping' smart constructor.
data ScheduledInstancesBlockDeviceMapping = ScheduledInstancesBlockDeviceMapping'
  { _sibdmVirtualName ::
      !(Maybe Text),
    _sibdmNoDevice ::
      !(Maybe Text),
    _sibdmEBS ::
      !( Maybe
           ScheduledInstancesEBS
       ),
    _sibdmDeviceName ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ScheduledInstancesBlockDeviceMapping' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sibdmVirtualName' - The virtual device name (@ephemeral@ N). Instance store volumes are numbered starting from 0. An instance type with two available instance store volumes can specify mappings for @ephemeral0@ and @ephemeral1@ . The number of available instance store volumes depends on the instance type. After you connect to the instance, you must mount the volume. Constraints: For M3 instances, you must specify instance store volumes in the block device mapping for the instance. When you launch an M3 instance, we ignore any instance store volumes specified in the block device mapping for the AMI.
--
-- * 'sibdmNoDevice' - Suppresses the specified device included in the block device mapping of the AMI.
--
-- * 'sibdmEBS' - Parameters used to set up EBS volumes automatically when the instance is launched.
--
-- * 'sibdmDeviceName' - The device name (for example, @/dev/sdh@ or @xvdh@ ).
scheduledInstancesBlockDeviceMapping ::
  ScheduledInstancesBlockDeviceMapping
scheduledInstancesBlockDeviceMapping =
  ScheduledInstancesBlockDeviceMapping'
    { _sibdmVirtualName =
        Nothing,
      _sibdmNoDevice = Nothing,
      _sibdmEBS = Nothing,
      _sibdmDeviceName = Nothing
    }

-- | The virtual device name (@ephemeral@ N). Instance store volumes are numbered starting from 0. An instance type with two available instance store volumes can specify mappings for @ephemeral0@ and @ephemeral1@ . The number of available instance store volumes depends on the instance type. After you connect to the instance, you must mount the volume. Constraints: For M3 instances, you must specify instance store volumes in the block device mapping for the instance. When you launch an M3 instance, we ignore any instance store volumes specified in the block device mapping for the AMI.
sibdmVirtualName :: Lens' ScheduledInstancesBlockDeviceMapping (Maybe Text)
sibdmVirtualName = lens _sibdmVirtualName (\s a -> s {_sibdmVirtualName = a})

-- | Suppresses the specified device included in the block device mapping of the AMI.
sibdmNoDevice :: Lens' ScheduledInstancesBlockDeviceMapping (Maybe Text)
sibdmNoDevice = lens _sibdmNoDevice (\s a -> s {_sibdmNoDevice = a})

-- | Parameters used to set up EBS volumes automatically when the instance is launched.
sibdmEBS :: Lens' ScheduledInstancesBlockDeviceMapping (Maybe ScheduledInstancesEBS)
sibdmEBS = lens _sibdmEBS (\s a -> s {_sibdmEBS = a})

-- | The device name (for example, @/dev/sdh@ or @xvdh@ ).
sibdmDeviceName :: Lens' ScheduledInstancesBlockDeviceMapping (Maybe Text)
sibdmDeviceName = lens _sibdmDeviceName (\s a -> s {_sibdmDeviceName = a})

instance Hashable ScheduledInstancesBlockDeviceMapping

instance NFData ScheduledInstancesBlockDeviceMapping

instance ToQuery ScheduledInstancesBlockDeviceMapping where
  toQuery ScheduledInstancesBlockDeviceMapping' {..} =
    mconcat
      [ "VirtualName" =: _sibdmVirtualName,
        "NoDevice" =: _sibdmNoDevice,
        "Ebs" =: _sibdmEBS,
        "DeviceName" =: _sibdmDeviceName
      ]
