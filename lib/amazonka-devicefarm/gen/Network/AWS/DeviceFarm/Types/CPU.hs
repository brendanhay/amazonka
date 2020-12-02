{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.CPU
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.CPU where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the amount of CPU that an app is using on a physical device. Does not represent system-wide CPU usage.
--
--
--
-- /See:/ 'cpu' smart constructor.
data CPU = CPU'
  { _cpuFrequency :: !(Maybe Text),
    _cpuClock :: !(Maybe Double),
    _cpuArchitecture :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CPU' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpuFrequency' - The CPU's frequency.
--
-- * 'cpuClock' - The clock speed of the device's CPU, expressed in hertz (Hz). For example, a 1.2 GHz CPU is expressed as 1200000000.
--
-- * 'cpuArchitecture' - The CPU's architecture (for example, x86 or ARM).
cpu ::
  CPU
cpu =
  CPU'
    { _cpuFrequency = Nothing,
      _cpuClock = Nothing,
      _cpuArchitecture = Nothing
    }

-- | The CPU's frequency.
cpuFrequency :: Lens' CPU (Maybe Text)
cpuFrequency = lens _cpuFrequency (\s a -> s {_cpuFrequency = a})

-- | The clock speed of the device's CPU, expressed in hertz (Hz). For example, a 1.2 GHz CPU is expressed as 1200000000.
cpuClock :: Lens' CPU (Maybe Double)
cpuClock = lens _cpuClock (\s a -> s {_cpuClock = a})

-- | The CPU's architecture (for example, x86 or ARM).
cpuArchitecture :: Lens' CPU (Maybe Text)
cpuArchitecture = lens _cpuArchitecture (\s a -> s {_cpuArchitecture = a})

instance FromJSON CPU where
  parseJSON =
    withObject
      "CPU"
      ( \x ->
          CPU'
            <$> (x .:? "frequency") <*> (x .:? "clock") <*> (x .:? "architecture")
      )

instance Hashable CPU

instance NFData CPU
