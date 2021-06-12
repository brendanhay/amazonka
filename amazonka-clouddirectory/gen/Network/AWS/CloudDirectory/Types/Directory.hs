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
-- Module      : Network.AWS.CloudDirectory.Types.Directory
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.Directory where

import Network.AWS.CloudDirectory.Types.DirectoryState
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Directory structure that includes the directory name and directory ARN.
--
-- /See:/ 'newDirectory' smart constructor.
data Directory = Directory'
  { -- | The Amazon Resource Name (ARN) that is associated with the directory.
    -- For more information, see arns.
    directoryArn :: Core.Maybe Core.Text,
    -- | The state of the directory. Can be either @Enabled@, @Disabled@, or
    -- @Deleted@.
    state :: Core.Maybe DirectoryState,
    -- | The name of the directory.
    name :: Core.Maybe Core.Text,
    -- | The date and time when the directory was created.
    creationDateTime :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Directory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryArn', 'directory_directoryArn' - The Amazon Resource Name (ARN) that is associated with the directory.
-- For more information, see arns.
--
-- 'state', 'directory_state' - The state of the directory. Can be either @Enabled@, @Disabled@, or
-- @Deleted@.
--
-- 'name', 'directory_name' - The name of the directory.
--
-- 'creationDateTime', 'directory_creationDateTime' - The date and time when the directory was created.
newDirectory ::
  Directory
newDirectory =
  Directory'
    { directoryArn = Core.Nothing,
      state = Core.Nothing,
      name = Core.Nothing,
      creationDateTime = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) that is associated with the directory.
-- For more information, see arns.
directory_directoryArn :: Lens.Lens' Directory (Core.Maybe Core.Text)
directory_directoryArn = Lens.lens (\Directory' {directoryArn} -> directoryArn) (\s@Directory' {} a -> s {directoryArn = a} :: Directory)

-- | The state of the directory. Can be either @Enabled@, @Disabled@, or
-- @Deleted@.
directory_state :: Lens.Lens' Directory (Core.Maybe DirectoryState)
directory_state = Lens.lens (\Directory' {state} -> state) (\s@Directory' {} a -> s {state = a} :: Directory)

-- | The name of the directory.
directory_name :: Lens.Lens' Directory (Core.Maybe Core.Text)
directory_name = Lens.lens (\Directory' {name} -> name) (\s@Directory' {} a -> s {name = a} :: Directory)

-- | The date and time when the directory was created.
directory_creationDateTime :: Lens.Lens' Directory (Core.Maybe Core.UTCTime)
directory_creationDateTime = Lens.lens (\Directory' {creationDateTime} -> creationDateTime) (\s@Directory' {} a -> s {creationDateTime = a} :: Directory) Core.. Lens.mapping Core._Time

instance Core.FromJSON Directory where
  parseJSON =
    Core.withObject
      "Directory"
      ( \x ->
          Directory'
            Core.<$> (x Core..:? "DirectoryArn")
            Core.<*> (x Core..:? "State")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "CreationDateTime")
      )

instance Core.Hashable Directory

instance Core.NFData Directory
