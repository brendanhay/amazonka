{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.InferenceAcceleratorOverride
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.InferenceAcceleratorOverride where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details on an Elastic Inference accelerator task override. This parameter is used to override the Elastic Inference accelerator specified in the task definition. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-eia.html Working with Amazon Elastic Inference on Amazon ECS> in the /Amazon Elastic Container Service Developer Guide/ .
--
--
--
-- /See:/ 'inferenceAcceleratorOverride' smart constructor.
data InferenceAcceleratorOverride = InferenceAcceleratorOverride'
  { _iaoDeviceName ::
      !(Maybe Text),
    _iaoDeviceType :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InferenceAcceleratorOverride' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iaoDeviceName' - The Elastic Inference accelerator device name to override for the task. This parameter must match a @deviceName@ specified in the task definition.
--
-- * 'iaoDeviceType' - The Elastic Inference accelerator type to use.
inferenceAcceleratorOverride ::
  InferenceAcceleratorOverride
inferenceAcceleratorOverride =
  InferenceAcceleratorOverride'
    { _iaoDeviceName = Nothing,
      _iaoDeviceType = Nothing
    }

-- | The Elastic Inference accelerator device name to override for the task. This parameter must match a @deviceName@ specified in the task definition.
iaoDeviceName :: Lens' InferenceAcceleratorOverride (Maybe Text)
iaoDeviceName = lens _iaoDeviceName (\s a -> s {_iaoDeviceName = a})

-- | The Elastic Inference accelerator type to use.
iaoDeviceType :: Lens' InferenceAcceleratorOverride (Maybe Text)
iaoDeviceType = lens _iaoDeviceType (\s a -> s {_iaoDeviceType = a})

instance FromJSON InferenceAcceleratorOverride where
  parseJSON =
    withObject
      "InferenceAcceleratorOverride"
      ( \x ->
          InferenceAcceleratorOverride'
            <$> (x .:? "deviceName") <*> (x .:? "deviceType")
      )

instance Hashable InferenceAcceleratorOverride

instance NFData InferenceAcceleratorOverride

instance ToJSON InferenceAcceleratorOverride where
  toJSON InferenceAcceleratorOverride' {..} =
    object
      ( catMaybes
          [ ("deviceName" .=) <$> _iaoDeviceName,
            ("deviceType" .=) <$> _iaoDeviceType
          ]
      )
