{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FpgaInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FpgaInfo where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.FpgaDeviceInfo
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the FPGAs for the instance type.
--
--
--
-- /See:/ 'fpgaInfo' smart constructor.
data FpgaInfo = FpgaInfo'
  { _fiTotalFpgaMemoryInMiB :: !(Maybe Int),
    _fiFpgas :: !(Maybe [FpgaDeviceInfo])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FpgaInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fiTotalFpgaMemoryInMiB' - The total memory of all FPGA accelerators for the instance type.
--
-- * 'fiFpgas' - Describes the FPGAs for the instance type.
fpgaInfo ::
  FpgaInfo
fpgaInfo =
  FpgaInfo' {_fiTotalFpgaMemoryInMiB = Nothing, _fiFpgas = Nothing}

-- | The total memory of all FPGA accelerators for the instance type.
fiTotalFpgaMemoryInMiB :: Lens' FpgaInfo (Maybe Int)
fiTotalFpgaMemoryInMiB = lens _fiTotalFpgaMemoryInMiB (\s a -> s {_fiTotalFpgaMemoryInMiB = a})

-- | Describes the FPGAs for the instance type.
fiFpgas :: Lens' FpgaInfo [FpgaDeviceInfo]
fiFpgas = lens _fiFpgas (\s a -> s {_fiFpgas = a}) . _Default . _Coerce

instance FromXML FpgaInfo where
  parseXML x =
    FpgaInfo'
      <$> (x .@? "totalFpgaMemoryInMiB")
      <*> (x .@? "fpgas" .!@ mempty >>= may (parseXMLList "item"))

instance Hashable FpgaInfo

instance NFData FpgaInfo
