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
-- Module      : Amazonka.Route53Domains.DisableDomainTransferLock
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation removes the transfer lock on the domain (specifically the
-- @clientTransferProhibited@ status) to allow domain transfers. We
-- recommend you refrain from performing this action unless you intend to
-- transfer the domain to a different registrar. Successful submission
-- returns an operation ID that you can use to track the progress and
-- completion of the action. If the request is not completed successfully,
-- the domain registrant will be notified by email.
module Amazonka.Route53Domains.DisableDomainTransferLock
  ( -- * Creating a Request
    DisableDomainTransferLock (..),
    newDisableDomainTransferLock,

    -- * Request Lenses
    disableDomainTransferLock_domainName,

    -- * Destructuring the Response
    DisableDomainTransferLockResponse (..),
    newDisableDomainTransferLockResponse,

    -- * Response Lenses
    disableDomainTransferLockResponse_operationId,
    disableDomainTransferLockResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Domains.Types

-- | The DisableDomainTransferLock request includes the following element.
--
-- /See:/ 'newDisableDomainTransferLock' smart constructor.
data DisableDomainTransferLock = DisableDomainTransferLock'
  { -- | The name of the domain that you want to remove the transfer lock for.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableDomainTransferLock' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'disableDomainTransferLock_domainName' - The name of the domain that you want to remove the transfer lock for.
newDisableDomainTransferLock ::
  -- | 'domainName'
  Prelude.Text ->
  DisableDomainTransferLock
newDisableDomainTransferLock pDomainName_ =
  DisableDomainTransferLock'
    { domainName =
        pDomainName_
    }

-- | The name of the domain that you want to remove the transfer lock for.
disableDomainTransferLock_domainName :: Lens.Lens' DisableDomainTransferLock Prelude.Text
disableDomainTransferLock_domainName = Lens.lens (\DisableDomainTransferLock' {domainName} -> domainName) (\s@DisableDomainTransferLock' {} a -> s {domainName = a} :: DisableDomainTransferLock)

instance Core.AWSRequest DisableDomainTransferLock where
  type
    AWSResponse DisableDomainTransferLock =
      DisableDomainTransferLockResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DisableDomainTransferLockResponse'
            Prelude.<$> (x Data..?> "OperationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisableDomainTransferLock where
  hashWithSalt _salt DisableDomainTransferLock' {..} =
    _salt `Prelude.hashWithSalt` domainName

instance Prelude.NFData DisableDomainTransferLock where
  rnf DisableDomainTransferLock' {..} =
    Prelude.rnf domainName

instance Data.ToHeaders DisableDomainTransferLock where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Domains_v20140515.DisableDomainTransferLock" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisableDomainTransferLock where
  toJSON DisableDomainTransferLock' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("DomainName" Data..= domainName)]
      )

instance Data.ToPath DisableDomainTransferLock where
  toPath = Prelude.const "/"

instance Data.ToQuery DisableDomainTransferLock where
  toQuery = Prelude.const Prelude.mempty

-- | The DisableDomainTransferLock response includes the following element.
--
-- /See:/ 'newDisableDomainTransferLockResponse' smart constructor.
data DisableDomainTransferLockResponse = DisableDomainTransferLockResponse'
  { -- | Identifier for tracking the progress of the request. To query the
    -- operation status, use
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail>.
    operationId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableDomainTransferLockResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operationId', 'disableDomainTransferLockResponse_operationId' - Identifier for tracking the progress of the request. To query the
-- operation status, use
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail>.
--
-- 'httpStatus', 'disableDomainTransferLockResponse_httpStatus' - The response's http status code.
newDisableDomainTransferLockResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisableDomainTransferLockResponse
newDisableDomainTransferLockResponse pHttpStatus_ =
  DisableDomainTransferLockResponse'
    { operationId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Identifier for tracking the progress of the request. To query the
-- operation status, use
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail>.
disableDomainTransferLockResponse_operationId :: Lens.Lens' DisableDomainTransferLockResponse (Prelude.Maybe Prelude.Text)
disableDomainTransferLockResponse_operationId = Lens.lens (\DisableDomainTransferLockResponse' {operationId} -> operationId) (\s@DisableDomainTransferLockResponse' {} a -> s {operationId = a} :: DisableDomainTransferLockResponse)

-- | The response's http status code.
disableDomainTransferLockResponse_httpStatus :: Lens.Lens' DisableDomainTransferLockResponse Prelude.Int
disableDomainTransferLockResponse_httpStatus = Lens.lens (\DisableDomainTransferLockResponse' {httpStatus} -> httpStatus) (\s@DisableDomainTransferLockResponse' {} a -> s {httpStatus = a} :: DisableDomainTransferLockResponse)

instance
  Prelude.NFData
    DisableDomainTransferLockResponse
  where
  rnf DisableDomainTransferLockResponse' {..} =
    Prelude.rnf operationId `Prelude.seq`
      Prelude.rnf httpStatus
