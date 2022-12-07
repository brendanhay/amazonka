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
-- Module      : Amazonka.CodeCommit.Types.Approval
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeCommit.Types.Approval where

import Amazonka.CodeCommit.Types.ApprovalState
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Returns information about a specific approval on a pull request.
--
-- /See:/ 'newApproval' smart constructor.
data Approval = Approval'
  { -- | The state of the approval, APPROVE or REVOKE. REVOKE states are not
    -- stored.
    approvalState :: Prelude.Maybe ApprovalState,
    -- | The Amazon Resource Name (ARN) of the user.
    userArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Approval' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'approvalState', 'approval_approvalState' - The state of the approval, APPROVE or REVOKE. REVOKE states are not
-- stored.
--
-- 'userArn', 'approval_userArn' - The Amazon Resource Name (ARN) of the user.
newApproval ::
  Approval
newApproval =
  Approval'
    { approvalState = Prelude.Nothing,
      userArn = Prelude.Nothing
    }

-- | The state of the approval, APPROVE or REVOKE. REVOKE states are not
-- stored.
approval_approvalState :: Lens.Lens' Approval (Prelude.Maybe ApprovalState)
approval_approvalState = Lens.lens (\Approval' {approvalState} -> approvalState) (\s@Approval' {} a -> s {approvalState = a} :: Approval)

-- | The Amazon Resource Name (ARN) of the user.
approval_userArn :: Lens.Lens' Approval (Prelude.Maybe Prelude.Text)
approval_userArn = Lens.lens (\Approval' {userArn} -> userArn) (\s@Approval' {} a -> s {userArn = a} :: Approval)

instance Data.FromJSON Approval where
  parseJSON =
    Data.withObject
      "Approval"
      ( \x ->
          Approval'
            Prelude.<$> (x Data..:? "approvalState")
            Prelude.<*> (x Data..:? "userArn")
      )

instance Prelude.Hashable Approval where
  hashWithSalt _salt Approval' {..} =
    _salt `Prelude.hashWithSalt` approvalState
      `Prelude.hashWithSalt` userArn

instance Prelude.NFData Approval where
  rnf Approval' {..} =
    Prelude.rnf approvalState
      `Prelude.seq` Prelude.rnf userArn
