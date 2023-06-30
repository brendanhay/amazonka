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
-- Module      : Amazonka.WorkDocs.Types.ShareResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkDocs.Types.ShareResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkDocs.Types.RoleType
import Amazonka.WorkDocs.Types.ShareStatusType

-- | Describes the share results of a resource.
--
-- /See:/ 'newShareResult' smart constructor.
data ShareResult = ShareResult'
  { -- | The ID of the invited user.
    inviteePrincipalId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the principal.
    principalId :: Prelude.Maybe Prelude.Text,
    -- | The role.
    role' :: Prelude.Maybe RoleType,
    -- | The ID of the resource that was shared.
    shareId :: Prelude.Maybe Prelude.Text,
    -- | The status.
    status :: Prelude.Maybe ShareStatusType,
    -- | The status message.
    statusMessage :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ShareResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inviteePrincipalId', 'shareResult_inviteePrincipalId' - The ID of the invited user.
--
-- 'principalId', 'shareResult_principalId' - The ID of the principal.
--
-- 'role'', 'shareResult_role' - The role.
--
-- 'shareId', 'shareResult_shareId' - The ID of the resource that was shared.
--
-- 'status', 'shareResult_status' - The status.
--
-- 'statusMessage', 'shareResult_statusMessage' - The status message.
newShareResult ::
  ShareResult
newShareResult =
  ShareResult'
    { inviteePrincipalId = Prelude.Nothing,
      principalId = Prelude.Nothing,
      role' = Prelude.Nothing,
      shareId = Prelude.Nothing,
      status = Prelude.Nothing,
      statusMessage = Prelude.Nothing
    }

-- | The ID of the invited user.
shareResult_inviteePrincipalId :: Lens.Lens' ShareResult (Prelude.Maybe Prelude.Text)
shareResult_inviteePrincipalId = Lens.lens (\ShareResult' {inviteePrincipalId} -> inviteePrincipalId) (\s@ShareResult' {} a -> s {inviteePrincipalId = a} :: ShareResult)

-- | The ID of the principal.
shareResult_principalId :: Lens.Lens' ShareResult (Prelude.Maybe Prelude.Text)
shareResult_principalId = Lens.lens (\ShareResult' {principalId} -> principalId) (\s@ShareResult' {} a -> s {principalId = a} :: ShareResult)

-- | The role.
shareResult_role :: Lens.Lens' ShareResult (Prelude.Maybe RoleType)
shareResult_role = Lens.lens (\ShareResult' {role'} -> role') (\s@ShareResult' {} a -> s {role' = a} :: ShareResult)

-- | The ID of the resource that was shared.
shareResult_shareId :: Lens.Lens' ShareResult (Prelude.Maybe Prelude.Text)
shareResult_shareId = Lens.lens (\ShareResult' {shareId} -> shareId) (\s@ShareResult' {} a -> s {shareId = a} :: ShareResult)

-- | The status.
shareResult_status :: Lens.Lens' ShareResult (Prelude.Maybe ShareStatusType)
shareResult_status = Lens.lens (\ShareResult' {status} -> status) (\s@ShareResult' {} a -> s {status = a} :: ShareResult)

-- | The status message.
shareResult_statusMessage :: Lens.Lens' ShareResult (Prelude.Maybe Prelude.Text)
shareResult_statusMessage = Lens.lens (\ShareResult' {statusMessage} -> statusMessage) (\s@ShareResult' {} a -> s {statusMessage = a} :: ShareResult) Prelude.. Lens.mapping Data._Sensitive

instance Data.FromJSON ShareResult where
  parseJSON =
    Data.withObject
      "ShareResult"
      ( \x ->
          ShareResult'
            Prelude.<$> (x Data..:? "InviteePrincipalId")
            Prelude.<*> (x Data..:? "PrincipalId")
            Prelude.<*> (x Data..:? "Role")
            Prelude.<*> (x Data..:? "ShareId")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "StatusMessage")
      )

instance Prelude.Hashable ShareResult where
  hashWithSalt _salt ShareResult' {..} =
    _salt
      `Prelude.hashWithSalt` inviteePrincipalId
      `Prelude.hashWithSalt` principalId
      `Prelude.hashWithSalt` role'
      `Prelude.hashWithSalt` shareId
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusMessage

instance Prelude.NFData ShareResult where
  rnf ShareResult' {..} =
    Prelude.rnf inviteePrincipalId
      `Prelude.seq` Prelude.rnf principalId
      `Prelude.seq` Prelude.rnf role'
      `Prelude.seq` Prelude.rnf shareId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusMessage
