{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.AttachedDisk
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.AttachedDisk where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a block storage disk that is attached to an instance, and is included in an automatic snapshot.
--
--
--
-- /See:/ 'attachedDisk' smart constructor.
data AttachedDisk = AttachedDisk'
  { _adPath :: !(Maybe Text),
    _adSizeInGb :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AttachedDisk' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adPath' - The path of the disk (e.g., @/dev/xvdf@ ).
--
-- * 'adSizeInGb' - The size of the disk in GB.
attachedDisk ::
  AttachedDisk
attachedDisk =
  AttachedDisk' {_adPath = Nothing, _adSizeInGb = Nothing}

-- | The path of the disk (e.g., @/dev/xvdf@ ).
adPath :: Lens' AttachedDisk (Maybe Text)
adPath = lens _adPath (\s a -> s {_adPath = a})

-- | The size of the disk in GB.
adSizeInGb :: Lens' AttachedDisk (Maybe Int)
adSizeInGb = lens _adSizeInGb (\s a -> s {_adSizeInGb = a})

instance FromJSON AttachedDisk where
  parseJSON =
    withObject
      "AttachedDisk"
      (\x -> AttachedDisk' <$> (x .:? "path") <*> (x .:? "sizeInGb"))

instance Hashable AttachedDisk

instance NFData AttachedDisk
