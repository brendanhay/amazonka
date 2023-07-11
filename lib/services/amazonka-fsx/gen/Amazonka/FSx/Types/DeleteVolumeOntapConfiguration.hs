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
-- Module      : Amazonka.FSx.Types.DeleteVolumeOntapConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.DeleteVolumeOntapConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Use to specify skipping a final backup, or to add tags to a final
-- backup.
--
-- /See:/ 'newDeleteVolumeOntapConfiguration' smart constructor.
data DeleteVolumeOntapConfiguration = DeleteVolumeOntapConfiguration'
  { finalBackupTags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | Set to true if you want to skip taking a final backup of the volume you
    -- are deleting.
    skipFinalBackup :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVolumeOntapConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'finalBackupTags', 'deleteVolumeOntapConfiguration_finalBackupTags' - Undocumented member.
--
-- 'skipFinalBackup', 'deleteVolumeOntapConfiguration_skipFinalBackup' - Set to true if you want to skip taking a final backup of the volume you
-- are deleting.
newDeleteVolumeOntapConfiguration ::
  DeleteVolumeOntapConfiguration
newDeleteVolumeOntapConfiguration =
  DeleteVolumeOntapConfiguration'
    { finalBackupTags =
        Prelude.Nothing,
      skipFinalBackup = Prelude.Nothing
    }

-- | Undocumented member.
deleteVolumeOntapConfiguration_finalBackupTags :: Lens.Lens' DeleteVolumeOntapConfiguration (Prelude.Maybe (Prelude.NonEmpty Tag))
deleteVolumeOntapConfiguration_finalBackupTags = Lens.lens (\DeleteVolumeOntapConfiguration' {finalBackupTags} -> finalBackupTags) (\s@DeleteVolumeOntapConfiguration' {} a -> s {finalBackupTags = a} :: DeleteVolumeOntapConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Set to true if you want to skip taking a final backup of the volume you
-- are deleting.
deleteVolumeOntapConfiguration_skipFinalBackup :: Lens.Lens' DeleteVolumeOntapConfiguration (Prelude.Maybe Prelude.Bool)
deleteVolumeOntapConfiguration_skipFinalBackup = Lens.lens (\DeleteVolumeOntapConfiguration' {skipFinalBackup} -> skipFinalBackup) (\s@DeleteVolumeOntapConfiguration' {} a -> s {skipFinalBackup = a} :: DeleteVolumeOntapConfiguration)

instance
  Prelude.Hashable
    DeleteVolumeOntapConfiguration
  where
  hashWithSalt
    _salt
    DeleteVolumeOntapConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` finalBackupTags
        `Prelude.hashWithSalt` skipFinalBackup

instance
  Prelude.NFData
    DeleteVolumeOntapConfiguration
  where
  rnf DeleteVolumeOntapConfiguration' {..} =
    Prelude.rnf finalBackupTags
      `Prelude.seq` Prelude.rnf skipFinalBackup

instance Data.ToJSON DeleteVolumeOntapConfiguration where
  toJSON DeleteVolumeOntapConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FinalBackupTags" Data..=)
              Prelude.<$> finalBackupTags,
            ("SkipFinalBackup" Data..=)
              Prelude.<$> skipFinalBackup
          ]
      )
