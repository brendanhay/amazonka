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
-- Module      : Amazonka.CloudDirectory.Types.Directory
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.Directory where

import Amazonka.CloudDirectory.Types.DirectoryState
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Directory structure that includes the directory name and directory ARN.
--
-- /See:/ 'newDirectory' smart constructor.
data Directory = Directory'
  { -- | The name of the directory.
    name :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the directory was created.
    creationDateTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) that is associated with the directory.
    -- For more information, see arns.
    directoryArn :: Prelude.Maybe Prelude.Text,
    -- | The state of the directory. Can be either @Enabled@, @Disabled@, or
    -- @Deleted@.
    state :: Prelude.Maybe DirectoryState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Directory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'directory_name' - The name of the directory.
--
-- 'creationDateTime', 'directory_creationDateTime' - The date and time when the directory was created.
--
-- 'directoryArn', 'directory_directoryArn' - The Amazon Resource Name (ARN) that is associated with the directory.
-- For more information, see arns.
--
-- 'state', 'directory_state' - The state of the directory. Can be either @Enabled@, @Disabled@, or
-- @Deleted@.
newDirectory ::
  Directory
newDirectory =
  Directory'
    { name = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      directoryArn = Prelude.Nothing,
      state = Prelude.Nothing
    }

-- | The name of the directory.
directory_name :: Lens.Lens' Directory (Prelude.Maybe Prelude.Text)
directory_name = Lens.lens (\Directory' {name} -> name) (\s@Directory' {} a -> s {name = a} :: Directory)

-- | The date and time when the directory was created.
directory_creationDateTime :: Lens.Lens' Directory (Prelude.Maybe Prelude.UTCTime)
directory_creationDateTime = Lens.lens (\Directory' {creationDateTime} -> creationDateTime) (\s@Directory' {} a -> s {creationDateTime = a} :: Directory) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) that is associated with the directory.
-- For more information, see arns.
directory_directoryArn :: Lens.Lens' Directory (Prelude.Maybe Prelude.Text)
directory_directoryArn = Lens.lens (\Directory' {directoryArn} -> directoryArn) (\s@Directory' {} a -> s {directoryArn = a} :: Directory)

-- | The state of the directory. Can be either @Enabled@, @Disabled@, or
-- @Deleted@.
directory_state :: Lens.Lens' Directory (Prelude.Maybe DirectoryState)
directory_state = Lens.lens (\Directory' {state} -> state) (\s@Directory' {} a -> s {state = a} :: Directory)

instance Data.FromJSON Directory where
  parseJSON =
    Data.withObject
      "Directory"
      ( \x ->
          Directory'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "CreationDateTime")
            Prelude.<*> (x Data..:? "DirectoryArn")
            Prelude.<*> (x Data..:? "State")
      )

instance Prelude.Hashable Directory where
  hashWithSalt _salt Directory' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` directoryArn
      `Prelude.hashWithSalt` state

instance Prelude.NFData Directory where
  rnf Directory' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf directoryArn
      `Prelude.seq` Prelude.rnf state
