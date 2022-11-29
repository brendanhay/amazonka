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
-- Module      : Amazonka.WellArchitected.Types.ShareInvitation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.ShareInvitation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.WellArchitected.Types.ShareResourceType

-- | The share invitation.
--
-- /See:/ 'newShareInvitation' smart constructor.
data ShareInvitation = ShareInvitation'
  { -- | The ARN for the lens.
    lensArn :: Prelude.Maybe Prelude.Text,
    lensAlias :: Prelude.Maybe Prelude.Text,
    -- | The ID assigned to the share invitation.
    shareInvitationId :: Prelude.Maybe Prelude.Text,
    -- | The resource type of the share invitation.
    shareResourceType :: Prelude.Maybe ShareResourceType,
    workloadId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ShareInvitation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lensArn', 'shareInvitation_lensArn' - The ARN for the lens.
--
-- 'lensAlias', 'shareInvitation_lensAlias' - Undocumented member.
--
-- 'shareInvitationId', 'shareInvitation_shareInvitationId' - The ID assigned to the share invitation.
--
-- 'shareResourceType', 'shareInvitation_shareResourceType' - The resource type of the share invitation.
--
-- 'workloadId', 'shareInvitation_workloadId' - Undocumented member.
newShareInvitation ::
  ShareInvitation
newShareInvitation =
  ShareInvitation'
    { lensArn = Prelude.Nothing,
      lensAlias = Prelude.Nothing,
      shareInvitationId = Prelude.Nothing,
      shareResourceType = Prelude.Nothing,
      workloadId = Prelude.Nothing
    }

-- | The ARN for the lens.
shareInvitation_lensArn :: Lens.Lens' ShareInvitation (Prelude.Maybe Prelude.Text)
shareInvitation_lensArn = Lens.lens (\ShareInvitation' {lensArn} -> lensArn) (\s@ShareInvitation' {} a -> s {lensArn = a} :: ShareInvitation)

-- | Undocumented member.
shareInvitation_lensAlias :: Lens.Lens' ShareInvitation (Prelude.Maybe Prelude.Text)
shareInvitation_lensAlias = Lens.lens (\ShareInvitation' {lensAlias} -> lensAlias) (\s@ShareInvitation' {} a -> s {lensAlias = a} :: ShareInvitation)

-- | The ID assigned to the share invitation.
shareInvitation_shareInvitationId :: Lens.Lens' ShareInvitation (Prelude.Maybe Prelude.Text)
shareInvitation_shareInvitationId = Lens.lens (\ShareInvitation' {shareInvitationId} -> shareInvitationId) (\s@ShareInvitation' {} a -> s {shareInvitationId = a} :: ShareInvitation)

-- | The resource type of the share invitation.
shareInvitation_shareResourceType :: Lens.Lens' ShareInvitation (Prelude.Maybe ShareResourceType)
shareInvitation_shareResourceType = Lens.lens (\ShareInvitation' {shareResourceType} -> shareResourceType) (\s@ShareInvitation' {} a -> s {shareResourceType = a} :: ShareInvitation)

-- | Undocumented member.
shareInvitation_workloadId :: Lens.Lens' ShareInvitation (Prelude.Maybe Prelude.Text)
shareInvitation_workloadId = Lens.lens (\ShareInvitation' {workloadId} -> workloadId) (\s@ShareInvitation' {} a -> s {workloadId = a} :: ShareInvitation)

instance Core.FromJSON ShareInvitation where
  parseJSON =
    Core.withObject
      "ShareInvitation"
      ( \x ->
          ShareInvitation'
            Prelude.<$> (x Core..:? "LensArn")
            Prelude.<*> (x Core..:? "LensAlias")
            Prelude.<*> (x Core..:? "ShareInvitationId")
            Prelude.<*> (x Core..:? "ShareResourceType")
            Prelude.<*> (x Core..:? "WorkloadId")
      )

instance Prelude.Hashable ShareInvitation where
  hashWithSalt _salt ShareInvitation' {..} =
    _salt `Prelude.hashWithSalt` lensArn
      `Prelude.hashWithSalt` lensAlias
      `Prelude.hashWithSalt` shareInvitationId
      `Prelude.hashWithSalt` shareResourceType
      `Prelude.hashWithSalt` workloadId

instance Prelude.NFData ShareInvitation where
  rnf ShareInvitation' {..} =
    Prelude.rnf lensArn
      `Prelude.seq` Prelude.rnf lensAlias
      `Prelude.seq` Prelude.rnf shareInvitationId
      `Prelude.seq` Prelude.rnf shareResourceType
      `Prelude.seq` Prelude.rnf workloadId
