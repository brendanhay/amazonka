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
-- Module      : Network.AWS.CodeCommit.Types.Approval
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.Approval where

import Network.AWS.CodeCommit.Types.ApprovalState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Returns information about a specific approval on a pull request.
--
-- /See:/ 'newApproval' smart constructor.
data Approval = Approval'
  { -- | The Amazon Resource Name (ARN) of the user.
    userArn :: Prelude.Maybe Prelude.Text,
    -- | The state of the approval, APPROVE or REVOKE. REVOKE states are not
    -- stored.
    approvalState :: Prelude.Maybe ApprovalState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Approval' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userArn', 'approval_userArn' - The Amazon Resource Name (ARN) of the user.
--
-- 'approvalState', 'approval_approvalState' - The state of the approval, APPROVE or REVOKE. REVOKE states are not
-- stored.
newApproval ::
  Approval
newApproval =
  Approval'
    { userArn = Prelude.Nothing,
      approvalState = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the user.
approval_userArn :: Lens.Lens' Approval (Prelude.Maybe Prelude.Text)
approval_userArn = Lens.lens (\Approval' {userArn} -> userArn) (\s@Approval' {} a -> s {userArn = a} :: Approval)

-- | The state of the approval, APPROVE or REVOKE. REVOKE states are not
-- stored.
approval_approvalState :: Lens.Lens' Approval (Prelude.Maybe ApprovalState)
approval_approvalState = Lens.lens (\Approval' {approvalState} -> approvalState) (\s@Approval' {} a -> s {approvalState = a} :: Approval)

instance Prelude.FromJSON Approval where
  parseJSON =
    Prelude.withObject
      "Approval"
      ( \x ->
          Approval'
            Prelude.<$> (x Prelude..:? "userArn")
            Prelude.<*> (x Prelude..:? "approvalState")
      )

instance Prelude.Hashable Approval

instance Prelude.NFData Approval
