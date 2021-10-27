{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecurityHub.DisassociateMembers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the specified member accounts from the associated
-- administrator account.
--
-- Can be used to disassociate both accounts that are managed using
-- Organizations and accounts that were invited manually.
module Network.AWS.SecurityHub.DisassociateMembers
  ( -- * Creating a Request
    DisassociateMembers (..),
    newDisassociateMembers,

    -- * Request Lenses
    disassociateMembers_accountIds,

    -- * Destructuring the Response
    DisassociateMembersResponse (..),
    newDisassociateMembersResponse,

    -- * Response Lenses
    disassociateMembersResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SecurityHub.Types

-- | /See:/ 'newDisassociateMembers' smart constructor.
data DisassociateMembers = DisassociateMembers'
  { -- | The account IDs of the member accounts to disassociate from the
    -- administrator account.
    accountIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateMembers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountIds', 'disassociateMembers_accountIds' - The account IDs of the member accounts to disassociate from the
-- administrator account.
newDisassociateMembers ::
  DisassociateMembers
newDisassociateMembers =
  DisassociateMembers' {accountIds = Prelude.mempty}

-- | The account IDs of the member accounts to disassociate from the
-- administrator account.
disassociateMembers_accountIds :: Lens.Lens' DisassociateMembers [Prelude.Text]
disassociateMembers_accountIds = Lens.lens (\DisassociateMembers' {accountIds} -> accountIds) (\s@DisassociateMembers' {} a -> s {accountIds = a} :: DisassociateMembers) Prelude.. Lens.coerced

instance Core.AWSRequest DisassociateMembers where
  type
    AWSResponse DisassociateMembers =
      DisassociateMembersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateMembersResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisassociateMembers

instance Prelude.NFData DisassociateMembers

instance Core.ToHeaders DisassociateMembers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DisassociateMembers where
  toJSON DisassociateMembers' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("AccountIds" Core..= accountIds)]
      )

instance Core.ToPath DisassociateMembers where
  toPath = Prelude.const "/members/disassociate"

instance Core.ToQuery DisassociateMembers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateMembersResponse' smart constructor.
data DisassociateMembersResponse = DisassociateMembersResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateMembersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateMembersResponse_httpStatus' - The response's http status code.
newDisassociateMembersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateMembersResponse
newDisassociateMembersResponse pHttpStatus_ =
  DisassociateMembersResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
disassociateMembersResponse_httpStatus :: Lens.Lens' DisassociateMembersResponse Prelude.Int
disassociateMembersResponse_httpStatus = Lens.lens (\DisassociateMembersResponse' {httpStatus} -> httpStatus) (\s@DisassociateMembersResponse' {} a -> s {httpStatus = a} :: DisassociateMembersResponse)

instance Prelude.NFData DisassociateMembersResponse
