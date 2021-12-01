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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.ShareInvitation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The share invitation.
--
-- /See:/ 'newShareInvitation' smart constructor.
data ShareInvitation = ShareInvitation'
  { workloadId :: Prelude.Maybe Prelude.Text,
    -- | The ID assigned to the share invitation.
    shareInvitationId :: Prelude.Maybe Prelude.Text
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
-- 'workloadId', 'shareInvitation_workloadId' - Undocumented member.
--
-- 'shareInvitationId', 'shareInvitation_shareInvitationId' - The ID assigned to the share invitation.
newShareInvitation ::
  ShareInvitation
newShareInvitation =
  ShareInvitation'
    { workloadId = Prelude.Nothing,
      shareInvitationId = Prelude.Nothing
    }

-- | Undocumented member.
shareInvitation_workloadId :: Lens.Lens' ShareInvitation (Prelude.Maybe Prelude.Text)
shareInvitation_workloadId = Lens.lens (\ShareInvitation' {workloadId} -> workloadId) (\s@ShareInvitation' {} a -> s {workloadId = a} :: ShareInvitation)

-- | The ID assigned to the share invitation.
shareInvitation_shareInvitationId :: Lens.Lens' ShareInvitation (Prelude.Maybe Prelude.Text)
shareInvitation_shareInvitationId = Lens.lens (\ShareInvitation' {shareInvitationId} -> shareInvitationId) (\s@ShareInvitation' {} a -> s {shareInvitationId = a} :: ShareInvitation)

instance Core.FromJSON ShareInvitation where
  parseJSON =
    Core.withObject
      "ShareInvitation"
      ( \x ->
          ShareInvitation'
            Prelude.<$> (x Core..:? "WorkloadId")
            Prelude.<*> (x Core..:? "ShareInvitationId")
      )

instance Prelude.Hashable ShareInvitation where
  hashWithSalt salt' ShareInvitation' {..} =
    salt' `Prelude.hashWithSalt` shareInvitationId
      `Prelude.hashWithSalt` workloadId

instance Prelude.NFData ShareInvitation where
  rnf ShareInvitation' {..} =
    Prelude.rnf workloadId
      `Prelude.seq` Prelude.rnf shareInvitationId
