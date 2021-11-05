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
-- Module      : Network.AWS.Macie.AssociateMemberAccount
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a specified AWS account with Amazon Macie Classic as a member
-- account.
module Network.AWS.Macie.AssociateMemberAccount
  ( -- * Creating a Request
    AssociateMemberAccount (..),
    newAssociateMemberAccount,

    -- * Request Lenses
    associateMemberAccount_memberAccountId,

    -- * Destructuring the Response
    AssociateMemberAccountResponse (..),
    newAssociateMemberAccountResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Macie.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAssociateMemberAccount' smart constructor.
data AssociateMemberAccount = AssociateMemberAccount'
  { -- | The ID of the AWS account that you want to associate with Amazon Macie
    -- Classic as a member account.
    memberAccountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateMemberAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'memberAccountId', 'associateMemberAccount_memberAccountId' - The ID of the AWS account that you want to associate with Amazon Macie
-- Classic as a member account.
newAssociateMemberAccount ::
  -- | 'memberAccountId'
  Prelude.Text ->
  AssociateMemberAccount
newAssociateMemberAccount pMemberAccountId_ =
  AssociateMemberAccount'
    { memberAccountId =
        pMemberAccountId_
    }

-- | The ID of the AWS account that you want to associate with Amazon Macie
-- Classic as a member account.
associateMemberAccount_memberAccountId :: Lens.Lens' AssociateMemberAccount Prelude.Text
associateMemberAccount_memberAccountId = Lens.lens (\AssociateMemberAccount' {memberAccountId} -> memberAccountId) (\s@AssociateMemberAccount' {} a -> s {memberAccountId = a} :: AssociateMemberAccount)

instance Core.AWSRequest AssociateMemberAccount where
  type
    AWSResponse AssociateMemberAccount =
      AssociateMemberAccountResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      AssociateMemberAccountResponse'

instance Prelude.Hashable AssociateMemberAccount

instance Prelude.NFData AssociateMemberAccount

instance Core.ToHeaders AssociateMemberAccount where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "MacieService.AssociateMemberAccount" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON AssociateMemberAccount where
  toJSON AssociateMemberAccount' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("memberAccountId" Core..= memberAccountId)
          ]
      )

instance Core.ToPath AssociateMemberAccount where
  toPath = Prelude.const "/"

instance Core.ToQuery AssociateMemberAccount where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateMemberAccountResponse' smart constructor.
data AssociateMemberAccountResponse = AssociateMemberAccountResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateMemberAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAssociateMemberAccountResponse ::
  AssociateMemberAccountResponse
newAssociateMemberAccountResponse =
  AssociateMemberAccountResponse'

instance
  Prelude.NFData
    AssociateMemberAccountResponse
