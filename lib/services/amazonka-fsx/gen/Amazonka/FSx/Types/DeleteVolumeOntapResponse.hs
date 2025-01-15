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
-- Module      : Amazonka.FSx.Types.DeleteVolumeOntapResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.DeleteVolumeOntapResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | The response object for the Amazon FSx for NetApp ONTAP volume being
-- deleted in the @DeleteVolume@ operation.
--
-- /See:/ 'newDeleteVolumeOntapResponse' smart constructor.
data DeleteVolumeOntapResponse = DeleteVolumeOntapResponse'
  { finalBackupId :: Prelude.Maybe Prelude.Text,
    finalBackupTags :: Prelude.Maybe (Prelude.NonEmpty Tag)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVolumeOntapResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'finalBackupId', 'deleteVolumeOntapResponse_finalBackupId' - Undocumented member.
--
-- 'finalBackupTags', 'deleteVolumeOntapResponse_finalBackupTags' - Undocumented member.
newDeleteVolumeOntapResponse ::
  DeleteVolumeOntapResponse
newDeleteVolumeOntapResponse =
  DeleteVolumeOntapResponse'
    { finalBackupId =
        Prelude.Nothing,
      finalBackupTags = Prelude.Nothing
    }

-- | Undocumented member.
deleteVolumeOntapResponse_finalBackupId :: Lens.Lens' DeleteVolumeOntapResponse (Prelude.Maybe Prelude.Text)
deleteVolumeOntapResponse_finalBackupId = Lens.lens (\DeleteVolumeOntapResponse' {finalBackupId} -> finalBackupId) (\s@DeleteVolumeOntapResponse' {} a -> s {finalBackupId = a} :: DeleteVolumeOntapResponse)

-- | Undocumented member.
deleteVolumeOntapResponse_finalBackupTags :: Lens.Lens' DeleteVolumeOntapResponse (Prelude.Maybe (Prelude.NonEmpty Tag))
deleteVolumeOntapResponse_finalBackupTags = Lens.lens (\DeleteVolumeOntapResponse' {finalBackupTags} -> finalBackupTags) (\s@DeleteVolumeOntapResponse' {} a -> s {finalBackupTags = a} :: DeleteVolumeOntapResponse) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON DeleteVolumeOntapResponse where
  parseJSON =
    Data.withObject
      "DeleteVolumeOntapResponse"
      ( \x ->
          DeleteVolumeOntapResponse'
            Prelude.<$> (x Data..:? "FinalBackupId")
            Prelude.<*> (x Data..:? "FinalBackupTags")
      )

instance Prelude.Hashable DeleteVolumeOntapResponse where
  hashWithSalt _salt DeleteVolumeOntapResponse' {..} =
    _salt
      `Prelude.hashWithSalt` finalBackupId
      `Prelude.hashWithSalt` finalBackupTags

instance Prelude.NFData DeleteVolumeOntapResponse where
  rnf DeleteVolumeOntapResponse' {..} =
    Prelude.rnf finalBackupId `Prelude.seq`
      Prelude.rnf finalBackupTags
