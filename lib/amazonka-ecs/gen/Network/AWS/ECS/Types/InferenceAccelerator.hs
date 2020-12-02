{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.InferenceAccelerator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.InferenceAccelerator where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details on a Elastic Inference accelerator. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-eia.html Working with Amazon Elastic Inference on Amazon ECS> in the /Amazon Elastic Container Service Developer Guide/ .
--
--
--
-- /See:/ 'inferenceAccelerator' smart constructor.
data InferenceAccelerator = InferenceAccelerator'
  { _iaDeviceName ::
      !Text,
    _iaDeviceType :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InferenceAccelerator' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iaDeviceName' - The Elastic Inference accelerator device name. The @deviceName@ must also be referenced in a container definition as a 'ResourceRequirement' .
--
-- * 'iaDeviceType' - The Elastic Inference accelerator type to use.
inferenceAccelerator ::
  -- | 'iaDeviceName'
  Text ->
  -- | 'iaDeviceType'
  Text ->
  InferenceAccelerator
inferenceAccelerator pDeviceName_ pDeviceType_ =
  InferenceAccelerator'
    { _iaDeviceName = pDeviceName_,
      _iaDeviceType = pDeviceType_
    }

-- | The Elastic Inference accelerator device name. The @deviceName@ must also be referenced in a container definition as a 'ResourceRequirement' .
iaDeviceName :: Lens' InferenceAccelerator Text
iaDeviceName = lens _iaDeviceName (\s a -> s {_iaDeviceName = a})

-- | The Elastic Inference accelerator type to use.
iaDeviceType :: Lens' InferenceAccelerator Text
iaDeviceType = lens _iaDeviceType (\s a -> s {_iaDeviceType = a})

instance FromJSON InferenceAccelerator where
  parseJSON =
    withObject
      "InferenceAccelerator"
      ( \x ->
          InferenceAccelerator'
            <$> (x .: "deviceName") <*> (x .: "deviceType")
      )

instance Hashable InferenceAccelerator

instance NFData InferenceAccelerator

instance ToJSON InferenceAccelerator where
  toJSON InferenceAccelerator' {..} =
    object
      ( catMaybes
          [ Just ("deviceName" .= _iaDeviceName),
            Just ("deviceType" .= _iaDeviceType)
          ]
      )
