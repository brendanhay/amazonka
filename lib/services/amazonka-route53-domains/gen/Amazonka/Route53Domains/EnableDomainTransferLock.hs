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
-- Module      : Amazonka.Route53Domains.EnableDomainTransferLock
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation sets the transfer lock on the domain (specifically the
-- @clientTransferProhibited@ status) to prevent domain transfers.
-- Successful submission returns an operation ID that you can use to track
-- the progress and completion of the action. If the request is not
-- completed successfully, the domain registrant will be notified by email.
module Amazonka.Route53Domains.EnableDomainTransferLock
  ( -- * Creating a Request
    EnableDomainTransferLock (..),
    newEnableDomainTransferLock,

    -- * Request Lenses
    enableDomainTransferLock_domainName,

    -- * Destructuring the Response
    EnableDomainTransferLockResponse (..),
    newEnableDomainTransferLockResponse,

    -- * Response Lenses
    enableDomainTransferLockResponse_operationId,
    enableDomainTransferLockResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Domains.Types

-- | A request to set the transfer lock for the specified domain.
--
-- /See:/ 'newEnableDomainTransferLock' smart constructor.
data EnableDomainTransferLock = EnableDomainTransferLock'
  { -- | The name of the domain that you want to set the transfer lock for.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  EnableDomainTransferLock
newEnableDomainTransferLock pDomainName_ =
  EnableDomainTransferLock'
    { domainName =
        pDomainName_
    }

-- | The name of the domain that you want to set the transfer lock for.
enableDomainTransferLock_domainName :: Lens.Lens' EnableDomainTransferLock Prelude.Text
enableDomainTransferLock_domainName = Lens.lens (\EnableDomainTransferLock' {domainName} -> domainName) (\s@EnableDomainTransferLock' {} a -> s {domainName = a} :: EnableDomainTransferLock)

instance Core.AWSRequest EnableDomainTransferLock where
  type
    AWSResponse EnableDomainTransferLock =
      EnableDomainTransferLockResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          EnableDomainTransferLockResponse'
            Prelude.<$> (x Data..?> "OperationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable EnableDomainTransferLock where
  hashWithSalt _salt EnableDomainTransferLock' {..} =
    _salt `Prelude.hashWithSalt` domainName

instance Prelude.NFData EnableDomainTransferLock where
  rnf EnableDomainTransferLock' {..} =
    Prelude.rnf domainName

instance Data.ToHeaders EnableDomainTransferLock where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Domains_v20140515.EnableDomainTransferLock" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON EnableDomainTransferLock where
  toJSON EnableDomainTransferLock' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("DomainName" Data..= domainName)]
      )

instance Data.ToPath EnableDomainTransferLock where
  toPath = Prelude.const "/"

instance Data.ToQuery EnableDomainTransferLock where
  toQuery = Prelude.const Prelude.mempty

-- | The EnableDomainTransferLock response includes the following elements.
--
-- /See:/ 'newEnableDomainTransferLockResponse' smart constructor.
data EnableDomainTransferLockResponse = EnableDomainTransferLockResponse'
  { -- | Identifier for tracking the progress of the request. To use this ID to
    -- query the operation status, use GetOperationDetail.
    operationId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableDomainTransferLockResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operationId', 'enableDomainTransferLockResponse_operationId' - Identifier for tracking the progress of the request. To use this ID to
-- query the operation status, use GetOperationDetail.
--
-- 'httpStatus', 'enableDomainTransferLockResponse_httpStatus' - The response's http status code.
newEnableDomainTransferLockResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  EnableDomainTransferLockResponse
newEnableDomainTransferLockResponse pHttpStatus_ =
  EnableDomainTransferLockResponse'
    { operationId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Identifier for tracking the progress of the request. To use this ID to
-- query the operation status, use GetOperationDetail.
enableDomainTransferLockResponse_operationId :: Lens.Lens' EnableDomainTransferLockResponse (Prelude.Maybe Prelude.Text)
enableDomainTransferLockResponse_operationId = Lens.lens (\EnableDomainTransferLockResponse' {operationId} -> operationId) (\s@EnableDomainTransferLockResponse' {} a -> s {operationId = a} :: EnableDomainTransferLockResponse)

-- | The response's http status code.
enableDomainTransferLockResponse_httpStatus :: Lens.Lens' EnableDomainTransferLockResponse Prelude.Int
enableDomainTransferLockResponse_httpStatus = Lens.lens (\EnableDomainTransferLockResponse' {httpStatus} -> httpStatus) (\s@EnableDomainTransferLockResponse' {} a -> s {httpStatus = a} :: EnableDomainTransferLockResponse)

instance
  Prelude.NFData
    EnableDomainTransferLockResponse
  where
  rnf EnableDomainTransferLockResponse' {..} =
    Prelude.rnf operationId `Prelude.seq`
      Prelude.rnf httpStatus
