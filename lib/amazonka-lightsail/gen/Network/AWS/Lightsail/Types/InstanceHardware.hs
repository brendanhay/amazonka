{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.InstanceHardware
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.InstanceHardware where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types.Disk
import Network.AWS.Prelude

-- | Describes the hardware for the instance.
--
--
--
-- /See:/ 'instanceHardware' smart constructor.
data InstanceHardware = InstanceHardware'
  { _ihCpuCount ::
      !(Maybe Int),
    _ihDisks :: !(Maybe [Disk]),
    _ihRamSizeInGb :: !(Maybe Double)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceHardware' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ihCpuCount' - The number of vCPUs the instance has.
--
-- * 'ihDisks' - The disks attached to the instance.
--
-- * 'ihRamSizeInGb' - The amount of RAM in GB on the instance (e.g., @1.0@ ).
instanceHardware ::
  InstanceHardware
instanceHardware =
  InstanceHardware'
    { _ihCpuCount = Nothing,
      _ihDisks = Nothing,
      _ihRamSizeInGb = Nothing
    }

-- | The number of vCPUs the instance has.
ihCpuCount :: Lens' InstanceHardware (Maybe Int)
ihCpuCount = lens _ihCpuCount (\s a -> s {_ihCpuCount = a})

-- | The disks attached to the instance.
ihDisks :: Lens' InstanceHardware [Disk]
ihDisks = lens _ihDisks (\s a -> s {_ihDisks = a}) . _Default . _Coerce

-- | The amount of RAM in GB on the instance (e.g., @1.0@ ).
ihRamSizeInGb :: Lens' InstanceHardware (Maybe Double)
ihRamSizeInGb = lens _ihRamSizeInGb (\s a -> s {_ihRamSizeInGb = a})

instance FromJSON InstanceHardware where
  parseJSON =
    withObject
      "InstanceHardware"
      ( \x ->
          InstanceHardware'
            <$> (x .:? "cpuCount")
            <*> (x .:? "disks" .!= mempty)
            <*> (x .:? "ramSizeInGb")
      )

instance Hashable InstanceHardware

instance NFData InstanceHardware
