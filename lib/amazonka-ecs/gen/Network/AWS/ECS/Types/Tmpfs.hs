{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.Tmpfs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.Tmpfs where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The container path, mount options, and size of the tmpfs mount.
--
--
--
-- /See:/ 'tmpfs' smart constructor.
data Tmpfs = Tmpfs'
  { _tMountOptions :: !(Maybe [Text]),
    _tContainerPath :: !Text,
    _tSize :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Tmpfs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tMountOptions' - The list of tmpfs volume mount options. Valid values: @"defaults" | "ro" | "rw" | "suid" | "nosuid" | "dev" | "nodev" | "exec" | "noexec" | "sync" | "async" | "dirsync" | "remount" | "mand" | "nomand" | "atime" | "noatime" | "diratime" | "nodiratime" | "bind" | "rbind" | "unbindable" | "runbindable" | "private" | "rprivate" | "shared" | "rshared" | "slave" | "rslave" | "relatime" | "norelatime" | "strictatime" | "nostrictatime" | "mode" | "uid" | "gid" | "nr_inodes" | "nr_blocks" | "mpol"@
--
-- * 'tContainerPath' - The absolute file path where the tmpfs volume is to be mounted.
--
-- * 'tSize' - The maximum size (in MiB) of the tmpfs volume.
tmpfs ::
  -- | 'tContainerPath'
  Text ->
  -- | 'tSize'
  Int ->
  Tmpfs
tmpfs pContainerPath_ pSize_ =
  Tmpfs'
    { _tMountOptions = Nothing,
      _tContainerPath = pContainerPath_,
      _tSize = pSize_
    }

-- | The list of tmpfs volume mount options. Valid values: @"defaults" | "ro" | "rw" | "suid" | "nosuid" | "dev" | "nodev" | "exec" | "noexec" | "sync" | "async" | "dirsync" | "remount" | "mand" | "nomand" | "atime" | "noatime" | "diratime" | "nodiratime" | "bind" | "rbind" | "unbindable" | "runbindable" | "private" | "rprivate" | "shared" | "rshared" | "slave" | "rslave" | "relatime" | "norelatime" | "strictatime" | "nostrictatime" | "mode" | "uid" | "gid" | "nr_inodes" | "nr_blocks" | "mpol"@
tMountOptions :: Lens' Tmpfs [Text]
tMountOptions = lens _tMountOptions (\s a -> s {_tMountOptions = a}) . _Default . _Coerce

-- | The absolute file path where the tmpfs volume is to be mounted.
tContainerPath :: Lens' Tmpfs Text
tContainerPath = lens _tContainerPath (\s a -> s {_tContainerPath = a})

-- | The maximum size (in MiB) of the tmpfs volume.
tSize :: Lens' Tmpfs Int
tSize = lens _tSize (\s a -> s {_tSize = a})

instance FromJSON Tmpfs where
  parseJSON =
    withObject
      "Tmpfs"
      ( \x ->
          Tmpfs'
            <$> (x .:? "mountOptions" .!= mempty)
            <*> (x .: "containerPath")
            <*> (x .: "size")
      )

instance Hashable Tmpfs

instance NFData Tmpfs

instance ToJSON Tmpfs where
  toJSON Tmpfs' {..} =
    object
      ( catMaybes
          [ ("mountOptions" .=) <$> _tMountOptions,
            Just ("containerPath" .= _tContainerPath),
            Just ("size" .= _tSize)
          ]
      )
