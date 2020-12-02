{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FpgaDeviceMemoryInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FpgaDeviceMemoryInfo where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the memory for the FPGA accelerator for the instance type.
--
--
--
-- /See:/ 'fpgaDeviceMemoryInfo' smart constructor.
newtype FpgaDeviceMemoryInfo = FpgaDeviceMemoryInfo'
  { _fdmiSizeInMiB ::
      Maybe Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FpgaDeviceMemoryInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fdmiSizeInMiB' - The size of the memory available to the FPGA accelerator, in MiB.
fpgaDeviceMemoryInfo ::
  FpgaDeviceMemoryInfo
fpgaDeviceMemoryInfo =
  FpgaDeviceMemoryInfo' {_fdmiSizeInMiB = Nothing}

-- | The size of the memory available to the FPGA accelerator, in MiB.
fdmiSizeInMiB :: Lens' FpgaDeviceMemoryInfo (Maybe Int)
fdmiSizeInMiB = lens _fdmiSizeInMiB (\s a -> s {_fdmiSizeInMiB = a})

instance FromXML FpgaDeviceMemoryInfo where
  parseXML x = FpgaDeviceMemoryInfo' <$> (x .@? "sizeInMiB")

instance Hashable FpgaDeviceMemoryInfo

instance NFData FpgaDeviceMemoryInfo
