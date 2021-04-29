{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.FMS.DisassociateAdminAccount
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the account that has been set as the AWS Firewall Manager
-- administrator account. To set a different account as the administrator
-- account, you must submit an @AssociateAdminAccount@ request.
module Network.AWS.FMS.DisassociateAdminAccount
  ( -- * Creating a Request
    DisassociateAdminAccount (..),
    newDisassociateAdminAccount,

    -- * Destructuring the Response
    DisassociateAdminAccountResponse (..),
    newDisassociateAdminAccountResponse,
  )
where

import Network.AWS.FMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisassociateAdminAccount' smart constructor.
data DisassociateAdminAccount = DisassociateAdminAccount'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DisassociateAdminAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisassociateAdminAccount ::
  DisassociateAdminAccount
newDisassociateAdminAccount =
  DisassociateAdminAccount'

instance Prelude.AWSRequest DisassociateAdminAccount where
  type
    Rs DisassociateAdminAccount =
      DisassociateAdminAccountResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      DisassociateAdminAccountResponse'

instance Prelude.Hashable DisassociateAdminAccount

instance Prelude.NFData DisassociateAdminAccount

instance Prelude.ToHeaders DisassociateAdminAccount where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSFMS_20180101.DisassociateAdminAccount" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DisassociateAdminAccount where
  toJSON =
    Prelude.const (Prelude.Object Prelude.mempty)

instance Prelude.ToPath DisassociateAdminAccount where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DisassociateAdminAccount where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateAdminAccountResponse' smart constructor.
data DisassociateAdminAccountResponse = DisassociateAdminAccountResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
