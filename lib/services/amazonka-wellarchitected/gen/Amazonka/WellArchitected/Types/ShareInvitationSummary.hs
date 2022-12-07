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
-- Module      : Amazonka.WellArchitected.Types.ShareInvitationSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.ShareInvitationSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WellArchitected.Types.PermissionType
import Amazonka.WellArchitected.Types.ShareResourceType

-- | A share invitation summary return object.
--
-- /See:/ 'newShareInvitationSummary' smart constructor.
data ShareInvitationSummary = ShareInvitationSummary'
  { permissionType :: Prelude.Maybe PermissionType,
    -- | The ARN for the lens.
    lensArn :: Prelude.Maybe Prelude.Text,
    sharedWith :: Prelude.Maybe Prelude.Text,
    -- | The ID assigned to the share invitation.
    shareInvitationId :: Prelude.Maybe Prelude.Text,
    workloadName :: Prelude.Maybe Prelude.Text,
    lensName :: Prelude.Maybe Prelude.Text,
    sharedBy :: Prelude.Maybe Prelude.Text,
    -- | The resource type of the share invitation.
    shareResourceType :: Prelude.Maybe ShareResourceType,
    workloadId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ShareInvitationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'permissionType', 'shareInvitationSummary_permissionType' - Undocumented member.
--
-- 'lensArn', 'shareInvitationSummary_lensArn' - The ARN for the lens.
--
-- 'sharedWith', 'shareInvitationSummary_sharedWith' - Undocumented member.
--
-- 'shareInvitationId', 'shareInvitationSummary_shareInvitationId' - The ID assigned to the share invitation.
--
-- 'workloadName', 'shareInvitationSummary_workloadName' - Undocumented member.
--
-- 'lensName', 'shareInvitationSummary_lensName' - Undocumented member.
--
-- 'sharedBy', 'shareInvitationSummary_sharedBy' - Undocumented member.
--
-- 'shareResourceType', 'shareInvitationSummary_shareResourceType' - The resource type of the share invitation.
--
-- 'workloadId', 'shareInvitationSummary_workloadId' - Undocumented member.
newShareInvitationSummary ::
  ShareInvitationSummary
newShareInvitationSummary =
  ShareInvitationSummary'
    { permissionType =
        Prelude.Nothing,
      lensArn = Prelude.Nothing,
      sharedWith = Prelude.Nothing,
      shareInvitationId = Prelude.Nothing,
      workloadName = Prelude.Nothing,
      lensName = Prelude.Nothing,
      sharedBy = Prelude.Nothing,
      shareResourceType = Prelude.Nothing,
      workloadId = Prelude.Nothing
    }

-- | Undocumented member.
shareInvitationSummary_permissionType :: Lens.Lens' ShareInvitationSummary (Prelude.Maybe PermissionType)
shareInvitationSummary_permissionType = Lens.lens (\ShareInvitationSummary' {permissionType} -> permissionType) (\s@ShareInvitationSummary' {} a -> s {permissionType = a} :: ShareInvitationSummary)

-- | The ARN for the lens.
shareInvitationSummary_lensArn :: Lens.Lens' ShareInvitationSummary (Prelude.Maybe Prelude.Text)
shareInvitationSummary_lensArn = Lens.lens (\ShareInvitationSummary' {lensArn} -> lensArn) (\s@ShareInvitationSummary' {} a -> s {lensArn = a} :: ShareInvitationSummary)

-- | Undocumented member.
shareInvitationSummary_sharedWith :: Lens.Lens' ShareInvitationSummary (Prelude.Maybe Prelude.Text)
shareInvitationSummary_sharedWith = Lens.lens (\ShareInvitationSummary' {sharedWith} -> sharedWith) (\s@ShareInvitationSummary' {} a -> s {sharedWith = a} :: ShareInvitationSummary)

-- | The ID assigned to the share invitation.
shareInvitationSummary_shareInvitationId :: Lens.Lens' ShareInvitationSummary (Prelude.Maybe Prelude.Text)
shareInvitationSummary_shareInvitationId = Lens.lens (\ShareInvitationSummary' {shareInvitationId} -> shareInvitationId) (\s@ShareInvitationSummary' {} a -> s {shareInvitationId = a} :: ShareInvitationSummary)

-- | Undocumented member.
shareInvitationSummary_workloadName :: Lens.Lens' ShareInvitationSummary (Prelude.Maybe Prelude.Text)
shareInvitationSummary_workloadName = Lens.lens (\ShareInvitationSummary' {workloadName} -> workloadName) (\s@ShareInvitationSummary' {} a -> s {workloadName = a} :: ShareInvitationSummary)

-- | Undocumented member.
shareInvitationSummary_lensName :: Lens.Lens' ShareInvitationSummary (Prelude.Maybe Prelude.Text)
shareInvitationSummary_lensName = Lens.lens (\ShareInvitationSummary' {lensName} -> lensName) (\s@ShareInvitationSummary' {} a -> s {lensName = a} :: ShareInvitationSummary)

-- | Undocumented member.
shareInvitationSummary_sharedBy :: Lens.Lens' ShareInvitationSummary (Prelude.Maybe Prelude.Text)
shareInvitationSummary_sharedBy = Lens.lens (\ShareInvitationSummary' {sharedBy} -> sharedBy) (\s@ShareInvitationSummary' {} a -> s {sharedBy = a} :: ShareInvitationSummary)

-- | The resource type of the share invitation.
shareInvitationSummary_shareResourceType :: Lens.Lens' ShareInvitationSummary (Prelude.Maybe ShareResourceType)
shareInvitationSummary_shareResourceType = Lens.lens (\ShareInvitationSummary' {shareResourceType} -> shareResourceType) (\s@ShareInvitationSummary' {} a -> s {shareResourceType = a} :: ShareInvitationSummary)

-- | Undocumented member.
shareInvitationSummary_workloadId :: Lens.Lens' ShareInvitationSummary (Prelude.Maybe Prelude.Text)
shareInvitationSummary_workloadId = Lens.lens (\ShareInvitationSummary' {workloadId} -> workloadId) (\s@ShareInvitationSummary' {} a -> s {workloadId = a} :: ShareInvitationSummary)

instance Data.FromJSON ShareInvitationSummary where
  parseJSON =
    Data.withObject
      "ShareInvitationSummary"
      ( \x ->
          ShareInvitationSummary'
            Prelude.<$> (x Data..:? "PermissionType")
            Prelude.<*> (x Data..:? "LensArn")
            Prelude.<*> (x Data..:? "SharedWith")
            Prelude.<*> (x Data..:? "ShareInvitationId")
            Prelude.<*> (x Data..:? "WorkloadName")
            Prelude.<*> (x Data..:? "LensName")
            Prelude.<*> (x Data..:? "SharedBy")
            Prelude.<*> (x Data..:? "ShareResourceType")
            Prelude.<*> (x Data..:? "WorkloadId")
      )

instance Prelude.Hashable ShareInvitationSummary where
  hashWithSalt _salt ShareInvitationSummary' {..} =
    _salt `Prelude.hashWithSalt` permissionType
      `Prelude.hashWithSalt` lensArn
      `Prelude.hashWithSalt` sharedWith
      `Prelude.hashWithSalt` shareInvitationId
      `Prelude.hashWithSalt` workloadName
      `Prelude.hashWithSalt` lensName
      `Prelude.hashWithSalt` sharedBy
      `Prelude.hashWithSalt` shareResourceType
      `Prelude.hashWithSalt` workloadId

instance Prelude.NFData ShareInvitationSummary where
  rnf ShareInvitationSummary' {..} =
    Prelude.rnf permissionType
      `Prelude.seq` Prelude.rnf lensArn
      `Prelude.seq` Prelude.rnf sharedWith
      `Prelude.seq` Prelude.rnf shareInvitationId
      `Prelude.seq` Prelude.rnf workloadName
      `Prelude.seq` Prelude.rnf lensName
      `Prelude.seq` Prelude.rnf sharedBy
      `Prelude.seq` Prelude.rnf shareResourceType
      `Prelude.seq` Prelude.rnf workloadId
