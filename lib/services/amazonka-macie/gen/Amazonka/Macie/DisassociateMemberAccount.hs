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
-- Module      : Amazonka.Macie.DisassociateMemberAccount
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- (Discontinued) Removes the specified member account from Amazon Macie
-- Classic.
module Amazonka.Macie.DisassociateMemberAccount
  ( -- * Creating a Request
    DisassociateMemberAccount (..),
    newDisassociateMemberAccount,

    -- * Request Lenses
    disassociateMemberAccount_memberAccountId,

    -- * Destructuring the Response
    DisassociateMemberAccountResponse (..),
    newDisassociateMemberAccountResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Macie.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateMemberAccount' smart constructor.
data DisassociateMemberAccount = DisassociateMemberAccount'
  { -- | (Discontinued) The ID of the member account that you want to remove from
    -- Amazon Macie Classic.
    memberAccountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateMemberAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'memberAccountId', 'disassociateMemberAccount_memberAccountId' - (Discontinued) The ID of the member account that you want to remove from
-- Amazon Macie Classic.
newDisassociateMemberAccount ::
  -- | 'memberAccountId'
  Prelude.Text ->
  DisassociateMemberAccount
newDisassociateMemberAccount pMemberAccountId_ =
  DisassociateMemberAccount'
    { memberAccountId =
        pMemberAccountId_
    }

-- | (Discontinued) The ID of the member account that you want to remove from
-- Amazon Macie Classic.
disassociateMemberAccount_memberAccountId :: Lens.Lens' DisassociateMemberAccount Prelude.Text
disassociateMemberAccount_memberAccountId = Lens.lens (\DisassociateMemberAccount' {memberAccountId} -> memberAccountId) (\s@DisassociateMemberAccount' {} a -> s {memberAccountId = a} :: DisassociateMemberAccount)

instance Core.AWSRequest DisassociateMemberAccount where
  type
    AWSResponse DisassociateMemberAccount =
      DisassociateMemberAccountResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DisassociateMemberAccountResponse'

instance Prelude.Hashable DisassociateMemberAccount where
  hashWithSalt _salt DisassociateMemberAccount' {..} =
    _salt `Prelude.hashWithSalt` memberAccountId

instance Prelude.NFData DisassociateMemberAccount where
  rnf DisassociateMemberAccount' {..} =
    Prelude.rnf memberAccountId

instance Data.ToHeaders DisassociateMemberAccount where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "MacieService.DisassociateMemberAccount" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisassociateMemberAccount where
  toJSON DisassociateMemberAccount' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("memberAccountId" Data..= memberAccountId)
          ]
      )

instance Data.ToPath DisassociateMemberAccount where
  toPath = Prelude.const "/"

instance Data.ToQuery DisassociateMemberAccount where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateMemberAccountResponse' smart constructor.
data DisassociateMemberAccountResponse = DisassociateMemberAccountResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateMemberAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisassociateMemberAccountResponse ::
  DisassociateMemberAccountResponse
newDisassociateMemberAccountResponse =
  DisassociateMemberAccountResponse'

instance
  Prelude.NFData
    DisassociateMemberAccountResponse
  where
  rnf _ = ()
