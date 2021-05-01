{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WorkDocs.Types.RoleType
import Network.AWS.WorkDocs.Types.ShareStatusType

-- | Describes the share results of a resource.
--
-- /See:/ 'newShareResult' smart constructor.
data ShareResult = ShareResult'
  { -- | The status message.
    statusMessage :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The status.
    status :: Prelude.Maybe ShareStatusType,
    -- | The ID of the invited user.
    inviteePrincipalId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the resource that was shared.
    shareId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the principal.
    principalId :: Prelude.Maybe Prelude.Text,
    -- | The role.
    role' :: Prelude.Maybe RoleType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { statusMessage = Prelude.Nothing,
      status = Prelude.Nothing,
      inviteePrincipalId = Prelude.Nothing,
      shareId = Prelude.Nothing,
      principalId = Prelude.Nothing,
      role' = Prelude.Nothing
    }

-- | The status message.
shareResult_statusMessage :: Lens.Lens' ShareResult (Prelude.Maybe Prelude.Text)
shareResult_statusMessage = Lens.lens (\ShareResult' {statusMessage} -> statusMessage) (\s@ShareResult' {} a -> s {statusMessage = a} :: ShareResult) Prelude.. Lens.mapping Prelude._Sensitive

-- | The status.
shareResult_status :: Lens.Lens' ShareResult (Prelude.Maybe ShareStatusType)
shareResult_status = Lens.lens (\ShareResult' {status} -> status) (\s@ShareResult' {} a -> s {status = a} :: ShareResult)

-- | The ID of the invited user.
shareResult_inviteePrincipalId :: Lens.Lens' ShareResult (Prelude.Maybe Prelude.Text)
shareResult_inviteePrincipalId = Lens.lens (\ShareResult' {inviteePrincipalId} -> inviteePrincipalId) (\s@ShareResult' {} a -> s {inviteePrincipalId = a} :: ShareResult)

-- | The ID of the resource that was shared.
shareResult_shareId :: Lens.Lens' ShareResult (Prelude.Maybe Prelude.Text)
shareResult_shareId = Lens.lens (\ShareResult' {shareId} -> shareId) (\s@ShareResult' {} a -> s {shareId = a} :: ShareResult)

-- | The ID of the principal.
shareResult_principalId :: Lens.Lens' ShareResult (Prelude.Maybe Prelude.Text)
shareResult_principalId = Lens.lens (\ShareResult' {principalId} -> principalId) (\s@ShareResult' {} a -> s {principalId = a} :: ShareResult)

-- | The role.
shareResult_role :: Lens.Lens' ShareResult (Prelude.Maybe RoleType)
shareResult_role = Lens.lens (\ShareResult' {role'} -> role') (\s@ShareResult' {} a -> s {role' = a} :: ShareResult)

instance Prelude.FromJSON ShareResult where
  parseJSON =
    Prelude.withObject
      "ShareResult"
      ( \x ->
          ShareResult'
            Prelude.<$> (x Prelude..:? "StatusMessage")
            Prelude.<*> (x Prelude..:? "Status")
            Prelude.<*> (x Prelude..:? "InviteePrincipalId")
            Prelude.<*> (x Prelude..:? "ShareId")
            Prelude.<*> (x Prelude..:? "PrincipalId")
            Prelude.<*> (x Prelude..:? "Role")
      )

instance Prelude.Hashable ShareResult

instance Prelude.NFData ShareResult
