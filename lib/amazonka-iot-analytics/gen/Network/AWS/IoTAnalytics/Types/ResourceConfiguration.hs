{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.ResourceConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.ResourceConfiguration where

import Network.AWS.IoTAnalytics.Types.ComputeType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The configuration of the resource used to execute the @containerAction@ .
--
--
--
-- /See:/ 'resourceConfiguration' smart constructor.
data ResourceConfiguration = ResourceConfiguration'
  { _rcComputeType ::
      !ComputeType,
    _rcVolumeSizeInGB :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourceConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcComputeType' - The type of the compute resource used to execute the @containerAction@ . Possible values are: @ACU_1@ (vCPU=4, memory=16 GiB) or @ACU_2@ (vCPU=8, memory=32 GiB).
--
-- * 'rcVolumeSizeInGB' - The size, in GB, of the persistent storage available to the resource instance used to execute the @containerAction@ (min: 1, max: 50).
resourceConfiguration ::
  -- | 'rcComputeType'
  ComputeType ->
  -- | 'rcVolumeSizeInGB'
  Natural ->
  ResourceConfiguration
resourceConfiguration pComputeType_ pVolumeSizeInGB_ =
  ResourceConfiguration'
    { _rcComputeType = pComputeType_,
      _rcVolumeSizeInGB = _Nat # pVolumeSizeInGB_
    }

-- | The type of the compute resource used to execute the @containerAction@ . Possible values are: @ACU_1@ (vCPU=4, memory=16 GiB) or @ACU_2@ (vCPU=8, memory=32 GiB).
rcComputeType :: Lens' ResourceConfiguration ComputeType
rcComputeType = lens _rcComputeType (\s a -> s {_rcComputeType = a})

-- | The size, in GB, of the persistent storage available to the resource instance used to execute the @containerAction@ (min: 1, max: 50).
rcVolumeSizeInGB :: Lens' ResourceConfiguration Natural
rcVolumeSizeInGB = lens _rcVolumeSizeInGB (\s a -> s {_rcVolumeSizeInGB = a}) . _Nat

instance FromJSON ResourceConfiguration where
  parseJSON =
    withObject
      "ResourceConfiguration"
      ( \x ->
          ResourceConfiguration'
            <$> (x .: "computeType") <*> (x .: "volumeSizeInGB")
      )

instance Hashable ResourceConfiguration

instance NFData ResourceConfiguration

instance ToJSON ResourceConfiguration where
  toJSON ResourceConfiguration' {..} =
    object
      ( catMaybes
          [ Just ("computeType" .= _rcComputeType),
            Just ("volumeSizeInGB" .= _rcVolumeSizeInGB)
          ]
      )
