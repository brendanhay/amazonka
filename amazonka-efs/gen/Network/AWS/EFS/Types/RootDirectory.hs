{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.Types.RootDirectory
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EFS.Types.RootDirectory where

import qualified Network.AWS.Core as Core
import Network.AWS.EFS.Types.CreationInfo
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the directory on the Amazon EFS file system that the access
-- point provides access to. The access point exposes the specified file
-- system path as the root directory of your file system to applications
-- using the access point. NFS clients using the access point can only
-- access data in the access point\'s @RootDirectory@ and it\'s
-- subdirectories.
--
-- /See:/ 'newRootDirectory' smart constructor.
data RootDirectory = RootDirectory'
  { -- | (Optional) Specifies the POSIX IDs and permissions to apply to the
    -- access point\'s @RootDirectory@. If the @RootDirectory@ > @Path@
    -- specified does not exist, EFS creates the root directory using the
    -- @CreationInfo@ settings when a client connects to an access point. When
    -- specifying the @CreationInfo@, you must provide values for all
    -- properties.
    --
    -- If you do not provide @CreationInfo@ and the specified @RootDirectory@ >
    -- @Path@ does not exist, attempts to mount the file system using the
    -- access point will fail.
    creationInfo :: Prelude.Maybe CreationInfo,
    -- | Specifies the path on the EFS file system to expose as the root
    -- directory to NFS clients using the access point to access the EFS file
    -- system. A path can have up to four subdirectories. If the specified path
    -- does not exist, you are required to provide the @CreationInfo@.
    path :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RootDirectory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationInfo', 'rootDirectory_creationInfo' - (Optional) Specifies the POSIX IDs and permissions to apply to the
-- access point\'s @RootDirectory@. If the @RootDirectory@ > @Path@
-- specified does not exist, EFS creates the root directory using the
-- @CreationInfo@ settings when a client connects to an access point. When
-- specifying the @CreationInfo@, you must provide values for all
-- properties.
--
-- If you do not provide @CreationInfo@ and the specified @RootDirectory@ >
-- @Path@ does not exist, attempts to mount the file system using the
-- access point will fail.
--
-- 'path', 'rootDirectory_path' - Specifies the path on the EFS file system to expose as the root
-- directory to NFS clients using the access point to access the EFS file
-- system. A path can have up to four subdirectories. If the specified path
-- does not exist, you are required to provide the @CreationInfo@.
newRootDirectory ::
  RootDirectory
newRootDirectory =
  RootDirectory'
    { creationInfo = Prelude.Nothing,
      path = Prelude.Nothing
    }

-- | (Optional) Specifies the POSIX IDs and permissions to apply to the
-- access point\'s @RootDirectory@. If the @RootDirectory@ > @Path@
-- specified does not exist, EFS creates the root directory using the
-- @CreationInfo@ settings when a client connects to an access point. When
-- specifying the @CreationInfo@, you must provide values for all
-- properties.
--
-- If you do not provide @CreationInfo@ and the specified @RootDirectory@ >
-- @Path@ does not exist, attempts to mount the file system using the
-- access point will fail.
rootDirectory_creationInfo :: Lens.Lens' RootDirectory (Prelude.Maybe CreationInfo)
rootDirectory_creationInfo = Lens.lens (\RootDirectory' {creationInfo} -> creationInfo) (\s@RootDirectory' {} a -> s {creationInfo = a} :: RootDirectory)

-- | Specifies the path on the EFS file system to expose as the root
-- directory to NFS clients using the access point to access the EFS file
-- system. A path can have up to four subdirectories. If the specified path
-- does not exist, you are required to provide the @CreationInfo@.
rootDirectory_path :: Lens.Lens' RootDirectory (Prelude.Maybe Prelude.Text)
rootDirectory_path = Lens.lens (\RootDirectory' {path} -> path) (\s@RootDirectory' {} a -> s {path = a} :: RootDirectory)

instance Core.FromJSON RootDirectory where
  parseJSON =
    Core.withObject
      "RootDirectory"
      ( \x ->
          RootDirectory'
            Prelude.<$> (x Core..:? "CreationInfo")
            Prelude.<*> (x Core..:? "Path")
      )

instance Prelude.Hashable RootDirectory

instance Prelude.NFData RootDirectory

instance Core.ToJSON RootDirectory where
  toJSON RootDirectory' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CreationInfo" Core..=) Prelude.<$> creationInfo,
            ("Path" Core..=) Prelude.<$> path
          ]
      )
