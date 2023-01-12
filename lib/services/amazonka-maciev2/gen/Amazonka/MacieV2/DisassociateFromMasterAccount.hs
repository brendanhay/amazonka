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
-- Module      : Amazonka.MacieV2.DisassociateFromMasterAccount
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- (Deprecated) Disassociates a member account from its Amazon Macie
-- administrator account. This operation has been replaced by the
-- DisassociateFromAdministratorAccount operation.
module Amazonka.MacieV2.DisassociateFromMasterAccount
  ( -- * Creating a Request
    DisassociateFromMasterAccount (..),
    newDisassociateFromMasterAccount,

    -- * Destructuring the Response
    DisassociateFromMasterAccountResponse (..),
    newDisassociateFromMasterAccountResponse,

    -- * Response Lenses
    disassociateFromMasterAccountResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateFromMasterAccount' smart constructor.
data DisassociateFromMasterAccount = DisassociateFromMasterAccount'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateFromMasterAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisassociateFromMasterAccount ::
  DisassociateFromMasterAccount
newDisassociateFromMasterAccount =
  DisassociateFromMasterAccount'

instance
  Core.AWSRequest
    DisassociateFromMasterAccount
  where
  type
    AWSResponse DisassociateFromMasterAccount =
      DisassociateFromMasterAccountResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateFromMasterAccountResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociateFromMasterAccount
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData DisassociateFromMasterAccount where
  rnf _ = ()

instance Data.ToHeaders DisassociateFromMasterAccount where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisassociateFromMasterAccount where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath DisassociateFromMasterAccount where
  toPath = Prelude.const "/master/disassociate"

instance Data.ToQuery DisassociateFromMasterAccount where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateFromMasterAccountResponse' smart constructor.
data DisassociateFromMasterAccountResponse = DisassociateFromMasterAccountResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateFromMasterAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateFromMasterAccountResponse_httpStatus' - The response's http status code.
newDisassociateFromMasterAccountResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateFromMasterAccountResponse
newDisassociateFromMasterAccountResponse pHttpStatus_ =
  DisassociateFromMasterAccountResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
disassociateFromMasterAccountResponse_httpStatus :: Lens.Lens' DisassociateFromMasterAccountResponse Prelude.Int
disassociateFromMasterAccountResponse_httpStatus = Lens.lens (\DisassociateFromMasterAccountResponse' {httpStatus} -> httpStatus) (\s@DisassociateFromMasterAccountResponse' {} a -> s {httpStatus = a} :: DisassociateFromMasterAccountResponse)

instance
  Prelude.NFData
    DisassociateFromMasterAccountResponse
  where
  rnf DisassociateFromMasterAccountResponse' {..} =
    Prelude.rnf httpStatus
