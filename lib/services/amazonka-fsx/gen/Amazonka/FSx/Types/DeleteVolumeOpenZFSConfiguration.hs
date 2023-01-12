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
-- Module      : Amazonka.FSx.Types.DeleteVolumeOpenZFSConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.DeleteVolumeOpenZFSConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types.DeleteOpenZFSVolumeOption
import qualified Amazonka.Prelude as Prelude

-- | A value that specifies whether to delete all child volumes and
-- snapshots.
--
-- /See:/ 'newDeleteVolumeOpenZFSConfiguration' smart constructor.
data DeleteVolumeOpenZFSConfiguration = DeleteVolumeOpenZFSConfiguration'
  { -- | To delete the volume\'s child volumes, snapshots, and clones, use the
    -- string @DELETE_CHILD_VOLUMES_AND_SNAPSHOTS@.
    options :: Prelude.Maybe [DeleteOpenZFSVolumeOption]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVolumeOpenZFSConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'options', 'deleteVolumeOpenZFSConfiguration_options' - To delete the volume\'s child volumes, snapshots, and clones, use the
-- string @DELETE_CHILD_VOLUMES_AND_SNAPSHOTS@.
newDeleteVolumeOpenZFSConfiguration ::
  DeleteVolumeOpenZFSConfiguration
newDeleteVolumeOpenZFSConfiguration =
  DeleteVolumeOpenZFSConfiguration'
    { options =
        Prelude.Nothing
    }

-- | To delete the volume\'s child volumes, snapshots, and clones, use the
-- string @DELETE_CHILD_VOLUMES_AND_SNAPSHOTS@.
deleteVolumeOpenZFSConfiguration_options :: Lens.Lens' DeleteVolumeOpenZFSConfiguration (Prelude.Maybe [DeleteOpenZFSVolumeOption])
deleteVolumeOpenZFSConfiguration_options = Lens.lens (\DeleteVolumeOpenZFSConfiguration' {options} -> options) (\s@DeleteVolumeOpenZFSConfiguration' {} a -> s {options = a} :: DeleteVolumeOpenZFSConfiguration) Prelude.. Lens.mapping Lens.coerced

instance
  Prelude.Hashable
    DeleteVolumeOpenZFSConfiguration
  where
  hashWithSalt
    _salt
    DeleteVolumeOpenZFSConfiguration' {..} =
      _salt `Prelude.hashWithSalt` options

instance
  Prelude.NFData
    DeleteVolumeOpenZFSConfiguration
  where
  rnf DeleteVolumeOpenZFSConfiguration' {..} =
    Prelude.rnf options

instance Data.ToJSON DeleteVolumeOpenZFSConfiguration where
  toJSON DeleteVolumeOpenZFSConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Options" Data..=) Prelude.<$> options]
      )
