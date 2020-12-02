{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.BlockDeviceMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.BlockDeviceMapping where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types.EBSBlockDevice
import Network.AWS.Prelude

-- | Describes a block device mapping. This data type maps directly to the Amazon EC2 <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_BlockDeviceMapping.html BlockDeviceMapping> data type.
--
--
--
-- /See:/ 'blockDeviceMapping' smart constructor.
data BlockDeviceMapping = BlockDeviceMapping'
  { _bdmVirtualName ::
      !(Maybe Text),
    _bdmNoDevice :: !(Maybe Text),
    _bdmEBS :: !(Maybe EBSBlockDevice),
    _bdmDeviceName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BlockDeviceMapping' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdmVirtualName' - The virtual device name. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_BlockDeviceMapping.html BlockDeviceMapping> .
--
-- * 'bdmNoDevice' - Suppresses the specified device included in the AMI's block device mapping.
--
-- * 'bdmEBS' - An @EBSBlockDevice@ that defines how to configure an Amazon EBS volume when the instance is launched.
--
-- * 'bdmDeviceName' - The device name that is exposed to the instance, such as @/dev/sdh@ . For the root device, you can use the explicit device name or you can set this parameter to @ROOT_DEVICE@ and AWS OpsWorks Stacks will provide the correct device name.
blockDeviceMapping ::
  BlockDeviceMapping
blockDeviceMapping =
  BlockDeviceMapping'
    { _bdmVirtualName = Nothing,
      _bdmNoDevice = Nothing,
      _bdmEBS = Nothing,
      _bdmDeviceName = Nothing
    }

-- | The virtual device name. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_BlockDeviceMapping.html BlockDeviceMapping> .
bdmVirtualName :: Lens' BlockDeviceMapping (Maybe Text)
bdmVirtualName = lens _bdmVirtualName (\s a -> s {_bdmVirtualName = a})

-- | Suppresses the specified device included in the AMI's block device mapping.
bdmNoDevice :: Lens' BlockDeviceMapping (Maybe Text)
bdmNoDevice = lens _bdmNoDevice (\s a -> s {_bdmNoDevice = a})

-- | An @EBSBlockDevice@ that defines how to configure an Amazon EBS volume when the instance is launched.
bdmEBS :: Lens' BlockDeviceMapping (Maybe EBSBlockDevice)
bdmEBS = lens _bdmEBS (\s a -> s {_bdmEBS = a})

-- | The device name that is exposed to the instance, such as @/dev/sdh@ . For the root device, you can use the explicit device name or you can set this parameter to @ROOT_DEVICE@ and AWS OpsWorks Stacks will provide the correct device name.
bdmDeviceName :: Lens' BlockDeviceMapping (Maybe Text)
bdmDeviceName = lens _bdmDeviceName (\s a -> s {_bdmDeviceName = a})

instance FromJSON BlockDeviceMapping where
  parseJSON =
    withObject
      "BlockDeviceMapping"
      ( \x ->
          BlockDeviceMapping'
            <$> (x .:? "VirtualName")
            <*> (x .:? "NoDevice")
            <*> (x .:? "Ebs")
            <*> (x .:? "DeviceName")
      )

instance Hashable BlockDeviceMapping

instance NFData BlockDeviceMapping

instance ToJSON BlockDeviceMapping where
  toJSON BlockDeviceMapping' {..} =
    object
      ( catMaybes
          [ ("VirtualName" .=) <$> _bdmVirtualName,
            ("NoDevice" .=) <$> _bdmNoDevice,
            ("Ebs" .=) <$> _bdmEBS,
            ("DeviceName" .=) <$> _bdmDeviceName
          ]
      )
