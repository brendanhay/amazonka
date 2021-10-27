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
-- Module      : Network.AWS.MacieV2.GetMasterAccount
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- (Deprecated) Retrieves information about the Amazon Macie administrator
-- account for an account. This operation has been replaced by the
-- GetAdministratorAccount operation.
module Network.AWS.MacieV2.GetMasterAccount
  ( -- * Creating a Request
    GetMasterAccount (..),
    newGetMasterAccount,

    -- * Destructuring the Response
    GetMasterAccountResponse (..),
    newGetMasterAccountResponse,

    -- * Response Lenses
    getMasterAccountResponse_master,
    getMasterAccountResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MacieV2.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetMasterAccount' smart constructor.
data GetMasterAccount = GetMasterAccount'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMasterAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetMasterAccount ::
  GetMasterAccount
newGetMasterAccount = GetMasterAccount'

instance Core.AWSRequest GetMasterAccount where
  type
    AWSResponse GetMasterAccount =
      GetMasterAccountResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMasterAccountResponse'
            Prelude.<$> (x Core..?> "master")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetMasterAccount

instance Prelude.NFData GetMasterAccount

instance Core.ToHeaders GetMasterAccount where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetMasterAccount where
  toPath = Prelude.const "/master"

instance Core.ToQuery GetMasterAccount where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetMasterAccountResponse' smart constructor.
data GetMasterAccountResponse = GetMasterAccountResponse'
  { -- | (Deprecated) The Amazon Web Services account ID for the administrator
    -- account. If the accounts are associated by a Macie membership
    -- invitation, this object also provides details about the invitation that
    -- was sent to establish the relationship between the accounts.
    master :: Prelude.Maybe Invitation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMasterAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'master', 'getMasterAccountResponse_master' - (Deprecated) The Amazon Web Services account ID for the administrator
-- account. If the accounts are associated by a Macie membership
-- invitation, this object also provides details about the invitation that
-- was sent to establish the relationship between the accounts.
--
-- 'httpStatus', 'getMasterAccountResponse_httpStatus' - The response's http status code.
newGetMasterAccountResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetMasterAccountResponse
newGetMasterAccountResponse pHttpStatus_ =
  GetMasterAccountResponse'
    { master = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | (Deprecated) The Amazon Web Services account ID for the administrator
-- account. If the accounts are associated by a Macie membership
-- invitation, this object also provides details about the invitation that
-- was sent to establish the relationship between the accounts.
getMasterAccountResponse_master :: Lens.Lens' GetMasterAccountResponse (Prelude.Maybe Invitation)
getMasterAccountResponse_master = Lens.lens (\GetMasterAccountResponse' {master} -> master) (\s@GetMasterAccountResponse' {} a -> s {master = a} :: GetMasterAccountResponse)

-- | The response's http status code.
getMasterAccountResponse_httpStatus :: Lens.Lens' GetMasterAccountResponse Prelude.Int
getMasterAccountResponse_httpStatus = Lens.lens (\GetMasterAccountResponse' {httpStatus} -> httpStatus) (\s@GetMasterAccountResponse' {} a -> s {httpStatus = a} :: GetMasterAccountResponse)

instance Prelude.NFData GetMasterAccountResponse
