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
-- Module      : Network.AWS.WorkDocs.Types.ShareResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.ShareResult where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.WorkDocs.Types.RoleType
import Network.AWS.WorkDocs.Types.ShareStatusType

-- | Describes the share results of a resource.
--
-- /See:/ 'newShareResult' smart constructor.
data ShareResult = ShareResult'
  { -- | The status message.
    statusMessage :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The status.
    status :: Core.Maybe ShareStatusType,
    -- | The ID of the invited user.
    inviteePrincipalId :: Core.Maybe Core.Text,
    -- | The ID of the resource that was shared.
    shareId :: Core.Maybe Core.Text,
    -- | The ID of the principal.
    principalId :: Core.Maybe Core.Text,
    -- | The role.
    role' :: Core.Maybe RoleType
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'ShareResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusMessage', 'shareResult_statusMessage' - The status message.
--
-- 'status', 'shareResult_status' - The status.
--
-- 'inviteePrincipalId', 'shareResult_inviteePrincipalId' - The ID of the invited user.
--
-- 'shareId', 'shareResult_shareId' - The ID of the resource that was shared.
--
-- 'principalId', 'shareResult_principalId' - The ID of the principal.
--
-- 'role'', 'shareResult_role' - The role.
newShareResult ::
  ShareResult
newShareResult =
  ShareResult'
    { statusMessage = Core.Nothing,
      status = Core.Nothing,
      inviteePrincipalId = Core.Nothing,
      shareId = Core.Nothing,
      principalId = Core.Nothing,
      role' = Core.Nothing
    }

-- | The status message.
shareResult_statusMessage :: Lens.Lens' ShareResult (Core.Maybe Core.Text)
shareResult_statusMessage = Lens.lens (\ShareResult' {statusMessage} -> statusMessage) (\s@ShareResult' {} a -> s {statusMessage = a} :: ShareResult) Core.. Lens.mapping Core._Sensitive

-- | The status.
shareResult_status :: Lens.Lens' ShareResult (Core.Maybe ShareStatusType)
shareResult_status = Lens.lens (\ShareResult' {status} -> status) (\s@ShareResult' {} a -> s {status = a} :: ShareResult)

-- | The ID of the invited user.
shareResult_inviteePrincipalId :: Lens.Lens' ShareResult (Core.Maybe Core.Text)
shareResult_inviteePrincipalId = Lens.lens (\ShareResult' {inviteePrincipalId} -> inviteePrincipalId) (\s@ShareResult' {} a -> s {inviteePrincipalId = a} :: ShareResult)

-- | The ID of the resource that was shared.
shareResult_shareId :: Lens.Lens' ShareResult (Core.Maybe Core.Text)
shareResult_shareId = Lens.lens (\ShareResult' {shareId} -> shareId) (\s@ShareResult' {} a -> s {shareId = a} :: ShareResult)

-- | The ID of the principal.
shareResult_principalId :: Lens.Lens' ShareResult (Core.Maybe Core.Text)
shareResult_principalId = Lens.lens (\ShareResult' {principalId} -> principalId) (\s@ShareResult' {} a -> s {principalId = a} :: ShareResult)

-- | The role.
shareResult_role :: Lens.Lens' ShareResult (Core.Maybe RoleType)
shareResult_role = Lens.lens (\ShareResult' {role'} -> role') (\s@ShareResult' {} a -> s {role' = a} :: ShareResult)

instance Core.FromJSON ShareResult where
  parseJSON =
    Core.withObject
      "ShareResult"
      ( \x ->
          ShareResult'
            Core.<$> (x Core..:? "StatusMessage")
            Core.<*> (x Core..:? "Status")
            Core.<*> (x Core..:? "InviteePrincipalId")
            Core.<*> (x Core..:? "ShareId")
            Core.<*> (x Core..:? "PrincipalId")
            Core.<*> (x Core..:? "Role")
      )

instance Core.Hashable ShareResult

instance Core.NFData ShareResult
