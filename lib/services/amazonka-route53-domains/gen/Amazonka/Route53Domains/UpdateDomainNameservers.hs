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
-- Module      : Amazonka.Route53Domains.UpdateDomainNameservers
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.Route53Domains.UpdateDomainNameservers
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Domains.Types

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
    fIAuthKey :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The name of the domain that you want to change name servers for.
    domainName :: Prelude.Text,
    -- | A list of new name servers for the domain.
    nameservers :: [Nameserver]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  UpdateDomainNameservers
newUpdateDomainNameservers pDomainName_ =
  UpdateDomainNameservers'
    { fIAuthKey =
        Prelude.Nothing,
      domainName = pDomainName_,
      nameservers = Prelude.mempty
    }

-- | The authorization key for .fi domains
updateDomainNameservers_fIAuthKey :: Lens.Lens' UpdateDomainNameservers (Prelude.Maybe Prelude.Text)
updateDomainNameservers_fIAuthKey = Lens.lens (\UpdateDomainNameservers' {fIAuthKey} -> fIAuthKey) (\s@UpdateDomainNameservers' {} a -> s {fIAuthKey = a} :: UpdateDomainNameservers) Prelude.. Lens.mapping Core._Sensitive

-- | The name of the domain that you want to change name servers for.
updateDomainNameservers_domainName :: Lens.Lens' UpdateDomainNameservers Prelude.Text
updateDomainNameservers_domainName = Lens.lens (\UpdateDomainNameservers' {domainName} -> domainName) (\s@UpdateDomainNameservers' {} a -> s {domainName = a} :: UpdateDomainNameservers)

-- | A list of new name servers for the domain.
updateDomainNameservers_nameservers :: Lens.Lens' UpdateDomainNameservers [Nameserver]
updateDomainNameservers_nameservers = Lens.lens (\UpdateDomainNameservers' {nameservers} -> nameservers) (\s@UpdateDomainNameservers' {} a -> s {nameservers = a} :: UpdateDomainNameservers) Prelude.. Lens.coerced

instance Core.AWSRequest UpdateDomainNameservers where
  type
    AWSResponse UpdateDomainNameservers =
      UpdateDomainNameserversResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDomainNameserversResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "OperationId")
      )

instance Prelude.Hashable UpdateDomainNameservers where
  hashWithSalt _salt UpdateDomainNameservers' {..} =
    _salt `Prelude.hashWithSalt` fIAuthKey
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` nameservers

instance Prelude.NFData UpdateDomainNameservers where
  rnf UpdateDomainNameservers' {..} =
    Prelude.rnf fIAuthKey
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf nameservers

instance Core.ToHeaders UpdateDomainNameservers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Route53Domains_v20140515.UpdateDomainNameservers" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateDomainNameservers where
  toJSON UpdateDomainNameservers' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("FIAuthKey" Core..=) Prelude.<$> fIAuthKey,
            Prelude.Just ("DomainName" Core..= domainName),
            Prelude.Just ("Nameservers" Core..= nameservers)
          ]
      )

instance Core.ToPath UpdateDomainNameservers where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateDomainNameservers where
  toQuery = Prelude.const Prelude.mempty

-- | The UpdateDomainNameservers response includes the following element.
--
-- /See:/ 'newUpdateDomainNameserversResponse' smart constructor.
data UpdateDomainNameserversResponse = UpdateDomainNameserversResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Identifier for tracking the progress of the request. To query the
    -- operation status, use
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail>.
    operationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  -- | 'operationId'
  Prelude.Text ->
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
updateDomainNameserversResponse_httpStatus :: Lens.Lens' UpdateDomainNameserversResponse Prelude.Int
updateDomainNameserversResponse_httpStatus = Lens.lens (\UpdateDomainNameserversResponse' {httpStatus} -> httpStatus) (\s@UpdateDomainNameserversResponse' {} a -> s {httpStatus = a} :: UpdateDomainNameserversResponse)

-- | Identifier for tracking the progress of the request. To query the
-- operation status, use
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail>.
updateDomainNameserversResponse_operationId :: Lens.Lens' UpdateDomainNameserversResponse Prelude.Text
updateDomainNameserversResponse_operationId = Lens.lens (\UpdateDomainNameserversResponse' {operationId} -> operationId) (\s@UpdateDomainNameserversResponse' {} a -> s {operationId = a} :: UpdateDomainNameserversResponse)

instance
  Prelude.NFData
    UpdateDomainNameserversResponse
  where
  rnf UpdateDomainNameserversResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf operationId
