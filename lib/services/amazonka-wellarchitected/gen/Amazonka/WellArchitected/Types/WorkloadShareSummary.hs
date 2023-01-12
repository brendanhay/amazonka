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
-- Module      : Amazonka.WellArchitected.Types.WorkloadShareSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.WorkloadShareSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WellArchitected.Types.PermissionType
import Amazonka.WellArchitected.Types.ShareStatus

-- | A workload share summary return object.
--
-- /See:/ 'newWorkloadShareSummary' smart constructor.
data WorkloadShareSummary = WorkloadShareSummary'
  { permissionType :: Prelude.Maybe PermissionType,
    shareId :: Prelude.Maybe Prelude.Text,
    sharedWith :: Prelude.Maybe Prelude.Text,
    status :: Prelude.Maybe ShareStatus,
    -- | Optional message to compliment the Status field.
    statusMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkloadShareSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'permissionType', 'workloadShareSummary_permissionType' - Undocumented member.
--
-- 'shareId', 'workloadShareSummary_shareId' - Undocumented member.
--
-- 'sharedWith', 'workloadShareSummary_sharedWith' - Undocumented member.
--
-- 'status', 'workloadShareSummary_status' - Undocumented member.
--
-- 'statusMessage', 'workloadShareSummary_statusMessage' - Optional message to compliment the Status field.
newWorkloadShareSummary ::
  WorkloadShareSummary
newWorkloadShareSummary =
  WorkloadShareSummary'
    { permissionType =
        Prelude.Nothing,
      shareId = Prelude.Nothing,
      sharedWith = Prelude.Nothing,
      status = Prelude.Nothing,
      statusMessage = Prelude.Nothing
    }

-- | Undocumented member.
workloadShareSummary_permissionType :: Lens.Lens' WorkloadShareSummary (Prelude.Maybe PermissionType)
workloadShareSummary_permissionType = Lens.lens (\WorkloadShareSummary' {permissionType} -> permissionType) (\s@WorkloadShareSummary' {} a -> s {permissionType = a} :: WorkloadShareSummary)

-- | Undocumented member.
workloadShareSummary_shareId :: Lens.Lens' WorkloadShareSummary (Prelude.Maybe Prelude.Text)
workloadShareSummary_shareId = Lens.lens (\WorkloadShareSummary' {shareId} -> shareId) (\s@WorkloadShareSummary' {} a -> s {shareId = a} :: WorkloadShareSummary)

-- | Undocumented member.
workloadShareSummary_sharedWith :: Lens.Lens' WorkloadShareSummary (Prelude.Maybe Prelude.Text)
workloadShareSummary_sharedWith = Lens.lens (\WorkloadShareSummary' {sharedWith} -> sharedWith) (\s@WorkloadShareSummary' {} a -> s {sharedWith = a} :: WorkloadShareSummary)

-- | Undocumented member.
workloadShareSummary_status :: Lens.Lens' WorkloadShareSummary (Prelude.Maybe ShareStatus)
workloadShareSummary_status = Lens.lens (\WorkloadShareSummary' {status} -> status) (\s@WorkloadShareSummary' {} a -> s {status = a} :: WorkloadShareSummary)

-- | Optional message to compliment the Status field.
workloadShareSummary_statusMessage :: Lens.Lens' WorkloadShareSummary (Prelude.Maybe Prelude.Text)
workloadShareSummary_statusMessage = Lens.lens (\WorkloadShareSummary' {statusMessage} -> statusMessage) (\s@WorkloadShareSummary' {} a -> s {statusMessage = a} :: WorkloadShareSummary)

instance Data.FromJSON WorkloadShareSummary where
  parseJSON =
    Data.withObject
      "WorkloadShareSummary"
      ( \x ->
          WorkloadShareSummary'
            Prelude.<$> (x Data..:? "PermissionType")
            Prelude.<*> (x Data..:? "ShareId")
            Prelude.<*> (x Data..:? "SharedWith")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "StatusMessage")
      )

instance Prelude.Hashable WorkloadShareSummary where
  hashWithSalt _salt WorkloadShareSummary' {..} =
    _salt `Prelude.hashWithSalt` permissionType
      `Prelude.hashWithSalt` shareId
      `Prelude.hashWithSalt` sharedWith
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusMessage

instance Prelude.NFData WorkloadShareSummary where
  rnf WorkloadShareSummary' {..} =
    Prelude.rnf permissionType
      `Prelude.seq` Prelude.rnf shareId
      `Prelude.seq` Prelude.rnf sharedWith
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusMessage
