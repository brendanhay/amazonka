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
-- Module      : Network.AWS.Route53Domains.DisableDomainTransferLock
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
module Network.AWS.Route53Domains.DisableDomainTransferLock
  ( -- * Creating a Request
    DisableDomainTransferLock (..),
    newDisableDomainTransferLock,

    -- * Request Lenses
    disableDomainTransferLock_domainName,

    -- * Destructuring the Response
    DisableDomainTransferLockResponse (..),
    newDisableDomainTransferLockResponse,

    -- * Response Lenses
    disableDomainTransferLockResponse_httpStatus,
    disableDomainTransferLockResponse_operationId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53Domains.Types

-- | The DisableDomainTransferLock request includes the following element.
--
-- /See:/ 'newDisableDomainTransferLock' smart constructor.
data DisableDomainTransferLock = DisableDomainTransferLock'
  { -- | The name of the domain that you want to remove the transfer lock for.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DisableDomainTransferLock where
  type
    Rs DisableDomainTransferLock =
      DisableDomainTransferLockResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DisableDomainTransferLockResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "OperationId")
      )

instance Prelude.Hashable DisableDomainTransferLock

instance Prelude.NFData DisableDomainTransferLock

instance Prelude.ToHeaders DisableDomainTransferLock where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Route53Domains_v20140515.DisableDomainTransferLock" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DisableDomainTransferLock where
  toJSON DisableDomainTransferLock' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("DomainName" Prelude..= domainName)]
      )

instance Prelude.ToPath DisableDomainTransferLock where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DisableDomainTransferLock where
  toQuery = Prelude.const Prelude.mempty

-- | The DisableDomainTransferLock response includes the following element.
--
-- /See:/ 'newDisableDomainTransferLockResponse' smart constructor.
data DisableDomainTransferLockResponse = DisableDomainTransferLockResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Identifier for tracking the progress of the request. To query the
    -- operation status, use
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail>.
    operationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DisableDomainTransferLockResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disableDomainTransferLockResponse_httpStatus' - The response's http status code.
--
-- 'operationId', 'disableDomainTransferLockResponse_operationId' - Identifier for tracking the progress of the request. To query the
-- operation status, use
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail>.
newDisableDomainTransferLockResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'operationId'
  Prelude.Text ->
  DisableDomainTransferLockResponse
newDisableDomainTransferLockResponse
  pHttpStatus_
  pOperationId_ =
    DisableDomainTransferLockResponse'
      { httpStatus =
          pHttpStatus_,
        operationId = pOperationId_
      }

-- | The response's http status code.
disableDomainTransferLockResponse_httpStatus :: Lens.Lens' DisableDomainTransferLockResponse Prelude.Int
disableDomainTransferLockResponse_httpStatus = Lens.lens (\DisableDomainTransferLockResponse' {httpStatus} -> httpStatus) (\s@DisableDomainTransferLockResponse' {} a -> s {httpStatus = a} :: DisableDomainTransferLockResponse)

-- | Identifier for tracking the progress of the request. To query the
-- operation status, use
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail>.
disableDomainTransferLockResponse_operationId :: Lens.Lens' DisableDomainTransferLockResponse Prelude.Text
disableDomainTransferLockResponse_operationId = Lens.lens (\DisableDomainTransferLockResponse' {operationId} -> operationId) (\s@DisableDomainTransferLockResponse' {} a -> s {operationId = a} :: DisableDomainTransferLockResponse)

instance
  Prelude.NFData
    DisableDomainTransferLockResponse
