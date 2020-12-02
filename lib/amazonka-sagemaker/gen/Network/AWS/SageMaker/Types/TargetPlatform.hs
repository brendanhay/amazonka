{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TargetPlatform
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TargetPlatform where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.TargetPlatformAccelerator
import Network.AWS.SageMaker.Types.TargetPlatformArch
import Network.AWS.SageMaker.Types.TargetPlatformOS

-- | Contains information about a target platform that you want your model to run on, such as OS, architecture, and accelerators. It is an alternative of @TargetDevice@ .
--
--
--
-- /See:/ 'targetPlatform' smart constructor.
data TargetPlatform = TargetPlatform'
  { _tpAccelerator ::
      !(Maybe TargetPlatformAccelerator),
    _tpOS :: !TargetPlatformOS,
    _tpArch :: !TargetPlatformArch
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TargetPlatform' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tpAccelerator' - Specifies a target platform accelerator (optional).     * @NVIDIA@ : Nvidia graphics processing unit. It also requires @gpu-code@ , @trt-ver@ , @cuda-ver@ compiler options     * @MALI@ : ARM Mali graphics processor     * @INTEL_GRAPHICS@ : Integrated Intel graphics
--
-- * 'tpOS' - Specifies a target platform OS.     * @LINUX@ : Linux-based operating systems.     * @ANDROID@ : Android operating systems. Android API level can be specified using the @ANDROID_PLATFORM@ compiler option. For example, @"CompilerOptions": {'ANDROID_PLATFORM': 28}@
--
-- * 'tpArch' - Specifies a target platform architecture.     * @X86_64@ : 64-bit version of the x86 instruction set.     * @X86@ : 32-bit version of the x86 instruction set.     * @ARM64@ : ARMv8 64-bit CPU.     * @ARM_EABIHF@ : ARMv7 32-bit, Hard Float.     * @ARM_EABI@ : ARMv7 32-bit, Soft Float. Used by Android 32-bit ARM platform.
targetPlatform ::
  -- | 'tpOS'
  TargetPlatformOS ->
  -- | 'tpArch'
  TargetPlatformArch ->
  TargetPlatform
targetPlatform pOS_ pArch_ =
  TargetPlatform'
    { _tpAccelerator = Nothing,
      _tpOS = pOS_,
      _tpArch = pArch_
    }

-- | Specifies a target platform accelerator (optional).     * @NVIDIA@ : Nvidia graphics processing unit. It also requires @gpu-code@ , @trt-ver@ , @cuda-ver@ compiler options     * @MALI@ : ARM Mali graphics processor     * @INTEL_GRAPHICS@ : Integrated Intel graphics
tpAccelerator :: Lens' TargetPlatform (Maybe TargetPlatformAccelerator)
tpAccelerator = lens _tpAccelerator (\s a -> s {_tpAccelerator = a})

-- | Specifies a target platform OS.     * @LINUX@ : Linux-based operating systems.     * @ANDROID@ : Android operating systems. Android API level can be specified using the @ANDROID_PLATFORM@ compiler option. For example, @"CompilerOptions": {'ANDROID_PLATFORM': 28}@
tpOS :: Lens' TargetPlatform TargetPlatformOS
tpOS = lens _tpOS (\s a -> s {_tpOS = a})

-- | Specifies a target platform architecture.     * @X86_64@ : 64-bit version of the x86 instruction set.     * @X86@ : 32-bit version of the x86 instruction set.     * @ARM64@ : ARMv8 64-bit CPU.     * @ARM_EABIHF@ : ARMv7 32-bit, Hard Float.     * @ARM_EABI@ : ARMv7 32-bit, Soft Float. Used by Android 32-bit ARM platform.
tpArch :: Lens' TargetPlatform TargetPlatformArch
tpArch = lens _tpArch (\s a -> s {_tpArch = a})

instance FromJSON TargetPlatform where
  parseJSON =
    withObject
      "TargetPlatform"
      ( \x ->
          TargetPlatform'
            <$> (x .:? "Accelerator") <*> (x .: "Os") <*> (x .: "Arch")
      )

instance Hashable TargetPlatform

instance NFData TargetPlatform

instance ToJSON TargetPlatform where
  toJSON TargetPlatform' {..} =
    object
      ( catMaybes
          [ ("Accelerator" .=) <$> _tpAccelerator,
            Just ("Os" .= _tpOS),
            Just ("Arch" .= _tpArch)
          ]
      )
