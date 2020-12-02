{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.DeviceMinutes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.DeviceMinutes where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the total (metered or unmetered) minutes used by the resource to run tests. Contains the sum of minutes consumed by all children.
--
--
--
-- /See:/ 'deviceMinutes' smart constructor.
data DeviceMinutes = DeviceMinutes'
  { _dmMetered :: !(Maybe Double),
    _dmTotal :: !(Maybe Double),
    _dmUnmetered :: !(Maybe Double)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeviceMinutes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmMetered' - When specified, represents only the sum of metered minutes used by the resource to run tests.
--
-- * 'dmTotal' - When specified, represents the total minutes used by the resource to run tests.
--
-- * 'dmUnmetered' - When specified, represents only the sum of unmetered minutes used by the resource to run tests.
deviceMinutes ::
  DeviceMinutes
deviceMinutes =
  DeviceMinutes'
    { _dmMetered = Nothing,
      _dmTotal = Nothing,
      _dmUnmetered = Nothing
    }

-- | When specified, represents only the sum of metered minutes used by the resource to run tests.
dmMetered :: Lens' DeviceMinutes (Maybe Double)
dmMetered = lens _dmMetered (\s a -> s {_dmMetered = a})

-- | When specified, represents the total minutes used by the resource to run tests.
dmTotal :: Lens' DeviceMinutes (Maybe Double)
dmTotal = lens _dmTotal (\s a -> s {_dmTotal = a})

-- | When specified, represents only the sum of unmetered minutes used by the resource to run tests.
dmUnmetered :: Lens' DeviceMinutes (Maybe Double)
dmUnmetered = lens _dmUnmetered (\s a -> s {_dmUnmetered = a})

instance FromJSON DeviceMinutes where
  parseJSON =
    withObject
      "DeviceMinutes"
      ( \x ->
          DeviceMinutes'
            <$> (x .:? "metered") <*> (x .:? "total") <*> (x .:? "unmetered")
      )

instance Hashable DeviceMinutes

instance NFData DeviceMinutes
