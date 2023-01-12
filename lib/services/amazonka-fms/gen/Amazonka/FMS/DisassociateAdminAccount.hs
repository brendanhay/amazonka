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
-- Module      : Amazonka.FMS.DisassociateAdminAccount
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the account that has been set as the Firewall Manager
-- administrator account. To set a different account as the administrator
-- account, you must submit an @AssociateAdminAccount@ request.
module Amazonka.FMS.DisassociateAdminAccount
  ( -- * Creating a Request
    DisassociateAdminAccount (..),
    newDisassociateAdminAccount,

    -- * Destructuring the Response
    DisassociateAdminAccountResponse (..),
    newDisassociateAdminAccountResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateAdminAccount' smart constructor.
data DisassociateAdminAccount = DisassociateAdminAccount'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateAdminAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisassociateAdminAccount ::
  DisassociateAdminAccount
newDisassociateAdminAccount =
  DisassociateAdminAccount'

instance Core.AWSRequest DisassociateAdminAccount where
  type
    AWSResponse DisassociateAdminAccount =
      DisassociateAdminAccountResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DisassociateAdminAccountResponse'

instance Prelude.Hashable DisassociateAdminAccount where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData DisassociateAdminAccount where
  rnf _ = ()

instance Data.ToHeaders DisassociateAdminAccount where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSFMS_20180101.DisassociateAdminAccount" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisassociateAdminAccount where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath DisassociateAdminAccount where
  toPath = Prelude.const "/"

instance Data.ToQuery DisassociateAdminAccount where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateAdminAccountResponse' smart constructor.
data DisassociateAdminAccountResponse = DisassociateAdminAccountResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateAdminAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisassociateAdminAccountResponse ::
  DisassociateAdminAccountResponse
newDisassociateAdminAccountResponse =
  DisassociateAdminAccountResponse'

instance
  Prelude.NFData
    DisassociateAdminAccountResponse
  where
  rnf _ = ()
