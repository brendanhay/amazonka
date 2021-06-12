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
-- Module      : Network.AWS.Route53Domains.UpdateDomainNameservers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation replaces the current set of name servers for the domain
-- with the specified set of name servers. If you use Amazon Route 53 as
-- your DNS service, specify the four name servers in the delegation set
-- for the hosted zone for the domain.
--
-- If successful, this operation returns an operation ID that you can use
-- to track the progress and completion of the action. If the request is
-- not completed successfully, the domain registrant will be notified by
-- email.
module Network.AWS.Route53Domains.UpdateDomainNameservers
  ( -- * Creating a Request
    UpdateDomainNameservers (..),
    newUpdateDomainNameservers,

    -- * Request Lenses
    updateDomainNameservers_fIAuthKey,
    updateDomainNameservers_domainName,
    updateDomainNameservers_nameservers,

    -- * Destructuring the Response
    UpdateDomainNameserversResponse (..),
    newUpdateDomainNameserversResponse,

    -- * Response Lenses
    updateDomainNameserversResponse_httpStatus,
    updateDomainNameserversResponse_operationId,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53Domains.Types

-- | Replaces the current set of name servers for the domain with the
-- specified set of name servers. If you use Amazon Route 53 as your DNS
-- service, specify the four name servers in the delegation set for the
-- hosted zone for the domain.
--
-- If successful, this operation returns an operation ID that you can use
-- to track the progress and completion of the action. If the request is
-- not completed successfully, the domain registrant will be notified by
-- email.
--
-- /See:/ 'newUpdateDomainNameservers' smart constructor.
data UpdateDomainNameservers = UpdateDomainNameservers'
  { -- | The authorization key for .fi domains
    fIAuthKey :: Core.Maybe Core.Text,
    -- | The name of the domain that you want to change name servers for.
    domainName :: Core.Text,
    -- | A list of new name servers for the domain.
    nameservers :: [Nameserver]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateDomainNameservers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fIAuthKey', 'updateDomainNameservers_fIAuthKey' - The authorization key for .fi domains
--
-- 'domainName', 'updateDomainNameservers_domainName' - The name of the domain that you want to change name servers for.
--
-- 'nameservers', 'updateDomainNameservers_nameservers' - A list of new name servers for the domain.
newUpdateDomainNameservers ::
  -- | 'domainName'
  Core.Text ->
  UpdateDomainNameservers
newUpdateDomainNameservers pDomainName_ =
  UpdateDomainNameservers'
    { fIAuthKey = Core.Nothing,
      domainName = pDomainName_,
      nameservers = Core.mempty
    }

-- | The authorization key for .fi domains
updateDomainNameservers_fIAuthKey :: Lens.Lens' UpdateDomainNameservers (Core.Maybe Core.Text)
updateDomainNameservers_fIAuthKey = Lens.lens (\UpdateDomainNameservers' {fIAuthKey} -> fIAuthKey) (\s@UpdateDomainNameservers' {} a -> s {fIAuthKey = a} :: UpdateDomainNameservers)

-- | The name of the domain that you want to change name servers for.
updateDomainNameservers_domainName :: Lens.Lens' UpdateDomainNameservers Core.Text
updateDomainNameservers_domainName = Lens.lens (\UpdateDomainNameservers' {domainName} -> domainName) (\s@UpdateDomainNameservers' {} a -> s {domainName = a} :: UpdateDomainNameservers)

-- | A list of new name servers for the domain.
updateDomainNameservers_nameservers :: Lens.Lens' UpdateDomainNameservers [Nameserver]
updateDomainNameservers_nameservers = Lens.lens (\UpdateDomainNameservers' {nameservers} -> nameservers) (\s@UpdateDomainNameservers' {} a -> s {nameservers = a} :: UpdateDomainNameservers) Core.. Lens._Coerce

instance Core.AWSRequest UpdateDomainNameservers where
  type
    AWSResponse UpdateDomainNameservers =
      UpdateDomainNameserversResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDomainNameserversResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "OperationId")
      )

instance Core.Hashable UpdateDomainNameservers

instance Core.NFData UpdateDomainNameservers

instance Core.ToHeaders UpdateDomainNameservers where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Route53Domains_v20140515.UpdateDomainNameservers" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateDomainNameservers where
  toJSON UpdateDomainNameservers' {..} =
    Core.object
      ( Core.catMaybes
          [ ("FIAuthKey" Core..=) Core.<$> fIAuthKey,
            Core.Just ("DomainName" Core..= domainName),
            Core.Just ("Nameservers" Core..= nameservers)
          ]
      )

instance Core.ToPath UpdateDomainNameservers where
  toPath = Core.const "/"

instance Core.ToQuery UpdateDomainNameservers where
  toQuery = Core.const Core.mempty

-- | The UpdateDomainNameservers response includes the following element.
--
-- /See:/ 'newUpdateDomainNameserversResponse' smart constructor.
data UpdateDomainNameserversResponse = UpdateDomainNameserversResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | Identifier for tracking the progress of the request. To query the
    -- operation status, use
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail>.
    operationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateDomainNameserversResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateDomainNameserversResponse_httpStatus' - The response's http status code.
--
-- 'operationId', 'updateDomainNameserversResponse_operationId' - Identifier for tracking the progress of the request. To query the
-- operation status, use
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail>.
newUpdateDomainNameserversResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'operationId'
  Core.Text ->
  UpdateDomainNameserversResponse
newUpdateDomainNameserversResponse
  pHttpStatus_
  pOperationId_ =
    UpdateDomainNameserversResponse'
      { httpStatus =
          pHttpStatus_,
        operationId = pOperationId_
      }

-- | The response's http status code.
updateDomainNameserversResponse_httpStatus :: Lens.Lens' UpdateDomainNameserversResponse Core.Int
updateDomainNameserversResponse_httpStatus = Lens.lens (\UpdateDomainNameserversResponse' {httpStatus} -> httpStatus) (\s@UpdateDomainNameserversResponse' {} a -> s {httpStatus = a} :: UpdateDomainNameserversResponse)

-- | Identifier for tracking the progress of the request. To query the
-- operation status, use
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail>.
updateDomainNameserversResponse_operationId :: Lens.Lens' UpdateDomainNameserversResponse Core.Text
updateDomainNameserversResponse_operationId = Lens.lens (\UpdateDomainNameserversResponse' {operationId} -> operationId) (\s@UpdateDomainNameserversResponse' {} a -> s {operationId = a} :: UpdateDomainNameserversResponse)

instance Core.NFData UpdateDomainNameserversResponse
