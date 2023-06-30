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
-- Module      : Amazonka.AppStream.DisassociateApplicationFromEntitlement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified application from the specified entitlement.
module Amazonka.AppStream.DisassociateApplicationFromEntitlement
  ( -- * Creating a Request
    DisassociateApplicationFromEntitlement (..),
    newDisassociateApplicationFromEntitlement,

    -- * Request Lenses
    disassociateApplicationFromEntitlement_stackName,
    disassociateApplicationFromEntitlement_entitlementName,
    disassociateApplicationFromEntitlement_applicationIdentifier,

    -- * Destructuring the Response
    DisassociateApplicationFromEntitlementResponse (..),
    newDisassociateApplicationFromEntitlementResponse,

    -- * Response Lenses
    disassociateApplicationFromEntitlementResponse_httpStatus,
  )
where

import Amazonka.AppStream.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateApplicationFromEntitlement' smart constructor.
data DisassociateApplicationFromEntitlement = DisassociateApplicationFromEntitlement'
  { -- | The name of the stack with which the entitlement is associated.
    stackName :: Prelude.Text,
    -- | The name of the entitlement.
    entitlementName :: Prelude.Text,
    -- | The identifier of the application to remove from the entitlement.
    applicationIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateApplicationFromEntitlement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackName', 'disassociateApplicationFromEntitlement_stackName' - The name of the stack with which the entitlement is associated.
--
-- 'entitlementName', 'disassociateApplicationFromEntitlement_entitlementName' - The name of the entitlement.
--
-- 'applicationIdentifier', 'disassociateApplicationFromEntitlement_applicationIdentifier' - The identifier of the application to remove from the entitlement.
newDisassociateApplicationFromEntitlement ::
  -- | 'stackName'
  Prelude.Text ->
  -- | 'entitlementName'
  Prelude.Text ->
  -- | 'applicationIdentifier'
  Prelude.Text ->
  DisassociateApplicationFromEntitlement
newDisassociateApplicationFromEntitlement
  pStackName_
  pEntitlementName_
  pApplicationIdentifier_ =
    DisassociateApplicationFromEntitlement'
      { stackName =
          pStackName_,
        entitlementName = pEntitlementName_,
        applicationIdentifier =
          pApplicationIdentifier_
      }

-- | The name of the stack with which the entitlement is associated.
disassociateApplicationFromEntitlement_stackName :: Lens.Lens' DisassociateApplicationFromEntitlement Prelude.Text
disassociateApplicationFromEntitlement_stackName = Lens.lens (\DisassociateApplicationFromEntitlement' {stackName} -> stackName) (\s@DisassociateApplicationFromEntitlement' {} a -> s {stackName = a} :: DisassociateApplicationFromEntitlement)

-- | The name of the entitlement.
disassociateApplicationFromEntitlement_entitlementName :: Lens.Lens' DisassociateApplicationFromEntitlement Prelude.Text
disassociateApplicationFromEntitlement_entitlementName = Lens.lens (\DisassociateApplicationFromEntitlement' {entitlementName} -> entitlementName) (\s@DisassociateApplicationFromEntitlement' {} a -> s {entitlementName = a} :: DisassociateApplicationFromEntitlement)

-- | The identifier of the application to remove from the entitlement.
disassociateApplicationFromEntitlement_applicationIdentifier :: Lens.Lens' DisassociateApplicationFromEntitlement Prelude.Text
disassociateApplicationFromEntitlement_applicationIdentifier = Lens.lens (\DisassociateApplicationFromEntitlement' {applicationIdentifier} -> applicationIdentifier) (\s@DisassociateApplicationFromEntitlement' {} a -> s {applicationIdentifier = a} :: DisassociateApplicationFromEntitlement)

instance
  Core.AWSRequest
    DisassociateApplicationFromEntitlement
  where
  type
    AWSResponse
      DisassociateApplicationFromEntitlement =
      DisassociateApplicationFromEntitlementResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateApplicationFromEntitlementResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociateApplicationFromEntitlement
  where
  hashWithSalt
    _salt
    DisassociateApplicationFromEntitlement' {..} =
      _salt
        `Prelude.hashWithSalt` stackName
        `Prelude.hashWithSalt` entitlementName
        `Prelude.hashWithSalt` applicationIdentifier

instance
  Prelude.NFData
    DisassociateApplicationFromEntitlement
  where
  rnf DisassociateApplicationFromEntitlement' {..} =
    Prelude.rnf stackName
      `Prelude.seq` Prelude.rnf entitlementName
      `Prelude.seq` Prelude.rnf applicationIdentifier

instance
  Data.ToHeaders
    DisassociateApplicationFromEntitlement
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PhotonAdminProxyService.DisassociateApplicationFromEntitlement" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DisassociateApplicationFromEntitlement
  where
  toJSON DisassociateApplicationFromEntitlement' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("StackName" Data..= stackName),
            Prelude.Just
              ("EntitlementName" Data..= entitlementName),
            Prelude.Just
              ( "ApplicationIdentifier"
                  Data..= applicationIdentifier
              )
          ]
      )

instance
  Data.ToPath
    DisassociateApplicationFromEntitlement
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DisassociateApplicationFromEntitlement
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateApplicationFromEntitlementResponse' smart constructor.
data DisassociateApplicationFromEntitlementResponse = DisassociateApplicationFromEntitlementResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateApplicationFromEntitlementResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateApplicationFromEntitlementResponse_httpStatus' - The response's http status code.
newDisassociateApplicationFromEntitlementResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateApplicationFromEntitlementResponse
newDisassociateApplicationFromEntitlementResponse
  pHttpStatus_ =
    DisassociateApplicationFromEntitlementResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
disassociateApplicationFromEntitlementResponse_httpStatus :: Lens.Lens' DisassociateApplicationFromEntitlementResponse Prelude.Int
disassociateApplicationFromEntitlementResponse_httpStatus = Lens.lens (\DisassociateApplicationFromEntitlementResponse' {httpStatus} -> httpStatus) (\s@DisassociateApplicationFromEntitlementResponse' {} a -> s {httpStatus = a} :: DisassociateApplicationFromEntitlementResponse)

instance
  Prelude.NFData
    DisassociateApplicationFromEntitlementResponse
  where
  rnf
    DisassociateApplicationFromEntitlementResponse' {..} =
      Prelude.rnf httpStatus
