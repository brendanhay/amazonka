{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FpgaDeviceInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FpgaDeviceInfo where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.FpgaDeviceMemoryInfo
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the FPGA accelerator for the instance type.
--
--
--
-- /See:/ 'fpgaDeviceInfo' smart constructor.
data FpgaDeviceInfo = FpgaDeviceInfo'
  { _fdiMemoryInfo ::
      !(Maybe FpgaDeviceMemoryInfo),
    _fdiManufacturer :: !(Maybe Text),
    _fdiCount :: !(Maybe Int),
    _fdiName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FpgaDeviceInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fdiMemoryInfo' - Describes the memory for the FPGA accelerator for the instance type.
--
-- * 'fdiManufacturer' - The manufacturer of the FPGA accelerator.
--
-- * 'fdiCount' - The count of FPGA accelerators for the instance type.
--
-- * 'fdiName' - The name of the FPGA accelerator.
fpgaDeviceInfo ::
  FpgaDeviceInfo
fpgaDeviceInfo =
  FpgaDeviceInfo'
    { _fdiMemoryInfo = Nothing,
      _fdiManufacturer = Nothing,
      _fdiCount = Nothing,
      _fdiName = Nothing
    }

-- | Describes the memory for the FPGA accelerator for the instance type.
fdiMemoryInfo :: Lens' FpgaDeviceInfo (Maybe FpgaDeviceMemoryInfo)
fdiMemoryInfo = lens _fdiMemoryInfo (\s a -> s {_fdiMemoryInfo = a})

-- | The manufacturer of the FPGA accelerator.
fdiManufacturer :: Lens' FpgaDeviceInfo (Maybe Text)
fdiManufacturer = lens _fdiManufacturer (\s a -> s {_fdiManufacturer = a})

-- | The count of FPGA accelerators for the instance type.
fdiCount :: Lens' FpgaDeviceInfo (Maybe Int)
fdiCount = lens _fdiCount (\s a -> s {_fdiCount = a})

-- | The name of the FPGA accelerator.
fdiName :: Lens' FpgaDeviceInfo (Maybe Text)
fdiName = lens _fdiName (\s a -> s {_fdiName = a})

instance FromXML FpgaDeviceInfo where
  parseXML x =
    FpgaDeviceInfo'
      <$> (x .@? "memoryInfo")
      <*> (x .@? "manufacturer")
      <*> (x .@? "count")
      <*> (x .@? "name")

instance Hashable FpgaDeviceInfo

instance NFData FpgaDeviceInfo
