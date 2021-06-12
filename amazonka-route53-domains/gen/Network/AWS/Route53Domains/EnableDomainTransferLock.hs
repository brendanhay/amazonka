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
-- Module      : Network.AWS.Route53Domains.EnableDomainTransferLock
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation sets the transfer lock on the domain (specifically the
-- @clientTransferProhibited@ status) to prevent domain transfers.
-- Successful submission returns an operation ID that you can use to track
-- the progress and completion of the action. If the request is not
-- completed successfully, the domain registrant will be notified by email.
module Network.AWS.Route53Domains.EnableDomainTransferLock
  ( -- * Creating a Request
    EnableDomainTransferLock (..),
    newEnableDomainTransferLock,

    -- * Request Lenses
    enableDomainTransferLock_domainName,

    -- * Destructuring the Response
    EnableDomainTransferLockResponse (..),
    newEnableDomainTransferLockResponse,

    -- * Response Lenses
    enableDomainTransferLockResponse_httpStatus,
    enableDomainTransferLockResponse_operationId,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53Domains.Types

-- | A request to set the transfer lock for the specified domain.
--
-- /See:/ 'newEnableDomainTransferLock' smart constructor.
data EnableDomainTransferLock = EnableDomainTransferLock'
  { -- | The name of the domain that you want to set the transfer lock for.
    domainName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EnableDomainTransferLock' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'enableDomainTransferLock_domainName' - The name of the domain that you want to set the transfer lock for.
newEnableDomainTransferLock ::
  -- | 'domainName'
  Core.Text ->
  EnableDomainTransferLock
newEnableDomainTransferLock pDomainName_ =
  EnableDomainTransferLock'
    { domainName =
        pDomainName_
    }

-- | The name of the domain that you want to set the transfer lock for.
enableDomainTransferLock_domainName :: Lens.Lens' EnableDomainTransferLock Core.Text
enableDomainTransferLock_domainName = Lens.lens (\EnableDomainTransferLock' {domainName} -> domainName) (\s@EnableDomainTransferLock' {} a -> s {domainName = a} :: EnableDomainTransferLock)

instance Core.AWSRequest EnableDomainTransferLock where
  type
    AWSResponse EnableDomainTransferLock =
      EnableDomainTransferLockResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          EnableDomainTransferLockResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "OperationId")
      )

instance Core.Hashable EnableDomainTransferLock

instance Core.NFData EnableDomainTransferLock

instance Core.ToHeaders EnableDomainTransferLock where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Route53Domains_v20140515.EnableDomainTransferLock" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON EnableDomainTransferLock where
  toJSON EnableDomainTransferLock' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("DomainName" Core..= domainName)]
      )

instance Core.ToPath EnableDomainTransferLock where
  toPath = Core.const "/"

instance Core.ToQuery EnableDomainTransferLock where
  toQuery = Core.const Core.mempty

-- | The EnableDomainTransferLock response includes the following elements.
--
-- /See:/ 'newEnableDomainTransferLockResponse' smart constructor.
data EnableDomainTransferLockResponse = EnableDomainTransferLockResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | Identifier for tracking the progress of the request. To use this ID to
    -- query the operation status, use GetOperationDetail.
    operationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EnableDomainTransferLockResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'enableDomainTransferLockResponse_httpStatus' - The response's http status code.
--
-- 'operationId', 'enableDomainTransferLockResponse_operationId' - Identifier for tracking the progress of the request. To use this ID to
-- query the operation status, use GetOperationDetail.
newEnableDomainTransferLockResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'operationId'
  Core.Text ->
  EnableDomainTransferLockResponse
newEnableDomainTransferLockResponse
  pHttpStatus_
  pOperationId_ =
    EnableDomainTransferLockResponse'
      { httpStatus =
          pHttpStatus_,
        operationId = pOperationId_
      }

-- | The response's http status code.
enableDomainTransferLockResponse_httpStatus :: Lens.Lens' EnableDomainTransferLockResponse Core.Int
enableDomainTransferLockResponse_httpStatus = Lens.lens (\EnableDomainTransferLockResponse' {httpStatus} -> httpStatus) (\s@EnableDomainTransferLockResponse' {} a -> s {httpStatus = a} :: EnableDomainTransferLockResponse)

-- | Identifier for tracking the progress of the request. To use this ID to
-- query the operation status, use GetOperationDetail.
enableDomainTransferLockResponse_operationId :: Lens.Lens' EnableDomainTransferLockResponse Core.Text
enableDomainTransferLockResponse_operationId = Lens.lens (\EnableDomainTransferLockResponse' {operationId} -> operationId) (\s@EnableDomainTransferLockResponse' {} a -> s {operationId = a} :: EnableDomainTransferLockResponse)

instance Core.NFData EnableDomainTransferLockResponse
