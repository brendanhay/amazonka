{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.OperatingSystem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.OperatingSystem where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WorkSpaces.Types.OperatingSystemType

-- | The operating system that the image is running.
--
--
--
-- /See:/ 'operatingSystem' smart constructor.
newtype OperatingSystem = OperatingSystem'
  { _osType ::
      Maybe OperatingSystemType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OperatingSystem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'osType' - The operating system.
operatingSystem ::
  OperatingSystem
operatingSystem = OperatingSystem' {_osType = Nothing}

-- | The operating system.
osType :: Lens' OperatingSystem (Maybe OperatingSystemType)
osType = lens _osType (\s a -> s {_osType = a})

instance FromJSON OperatingSystem where
  parseJSON =
    withObject
      "OperatingSystem"
      (\x -> OperatingSystem' <$> (x .:? "Type"))

instance Hashable OperatingSystem

instance NFData OperatingSystem
