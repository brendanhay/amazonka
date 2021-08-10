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
-- Module      : Network.AWS.Route53Domains.RenewDomain
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation renews a domain for the specified number of years. The
-- cost of renewing your domain is billed to your AWS account.
--
-- We recommend that you renew your domain several weeks before the
-- expiration date. Some TLD registries delete domains before the
-- expiration date if you haven\'t renewed far enough in advance. For more
-- information about renewing domain registration, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/domain-renew.html Renewing Registration for a Domain>
-- in the /Amazon Route 53 Developer Guide/.
module Network.AWS.Route53Domains.RenewDomain
  ( -- * Creating a Request
    RenewDomain (..),
    newRenewDomain,

    -- * Request Lenses
    renewDomain_durationInYears,
    renewDomain_domainName,
    renewDomain_currentExpiryYear,

    -- * Destructuring the Response
    RenewDomainResponse (..),
    newRenewDomainResponse,

    -- * Response Lenses
    renewDomainResponse_httpStatus,
    renewDomainResponse_operationId,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53Domains.Types

-- | A @RenewDomain@ request includes the number of years that you want to
-- renew for and the current expiration year.
--
-- /See:/ 'newRenewDomain' smart constructor.
data RenewDomain = RenewDomain'
  { -- | The number of years that you want to renew the domain for. The maximum
    -- number of years depends on the top-level domain. For the range of valid
    -- values for your domain, see
    -- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53>
    -- in the /Amazon Route 53 Developer Guide/.
    --
    -- Default: 1
    durationInYears :: Prelude.Maybe Prelude.Natural,
    -- | The name of the domain that you want to renew.
    domainName :: Prelude.Text,
    -- | The year when the registration for the domain is set to expire. This
    -- value must match the current expiration date for the domain.
    currentExpiryYear :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RenewDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'durationInYears', 'renewDomain_durationInYears' - The number of years that you want to renew the domain for. The maximum
-- number of years depends on the top-level domain. For the range of valid
-- values for your domain, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53>
-- in the /Amazon Route 53 Developer Guide/.
--
-- Default: 1
--
-- 'domainName', 'renewDomain_domainName' - The name of the domain that you want to renew.
--
-- 'currentExpiryYear', 'renewDomain_currentExpiryYear' - The year when the registration for the domain is set to expire. This
-- value must match the current expiration date for the domain.
newRenewDomain ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'currentExpiryYear'
  Prelude.Int ->
  RenewDomain
newRenewDomain pDomainName_ pCurrentExpiryYear_ =
  RenewDomain'
    { durationInYears = Prelude.Nothing,
      domainName = pDomainName_,
      currentExpiryYear = pCurrentExpiryYear_
    }

-- | The number of years that you want to renew the domain for. The maximum
-- number of years depends on the top-level domain. For the range of valid
-- values for your domain, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53>
-- in the /Amazon Route 53 Developer Guide/.
--
-- Default: 1
renewDomain_durationInYears :: Lens.Lens' RenewDomain (Prelude.Maybe Prelude.Natural)
renewDomain_durationInYears = Lens.lens (\RenewDomain' {durationInYears} -> durationInYears) (\s@RenewDomain' {} a -> s {durationInYears = a} :: RenewDomain)

-- | The name of the domain that you want to renew.
renewDomain_domainName :: Lens.Lens' RenewDomain Prelude.Text
renewDomain_domainName = Lens.lens (\RenewDomain' {domainName} -> domainName) (\s@RenewDomain' {} a -> s {domainName = a} :: RenewDomain)

-- | The year when the registration for the domain is set to expire. This
-- value must match the current expiration date for the domain.
renewDomain_currentExpiryYear :: Lens.Lens' RenewDomain Prelude.Int
renewDomain_currentExpiryYear = Lens.lens (\RenewDomain' {currentExpiryYear} -> currentExpiryYear) (\s@RenewDomain' {} a -> s {currentExpiryYear = a} :: RenewDomain)

instance Core.AWSRequest RenewDomain where
  type AWSResponse RenewDomain = RenewDomainResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RenewDomainResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "OperationId")
      )

instance Prelude.Hashable RenewDomain

instance Prelude.NFData RenewDomain

instance Core.ToHeaders RenewDomain where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Route53Domains_v20140515.RenewDomain" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON RenewDomain where
  toJSON RenewDomain' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DurationInYears" Core..=)
              Prelude.<$> durationInYears,
            Prelude.Just ("DomainName" Core..= domainName),
            Prelude.Just
              ("CurrentExpiryYear" Core..= currentExpiryYear)
          ]
      )

instance Core.ToPath RenewDomain where
  toPath = Prelude.const "/"

instance Core.ToQuery RenewDomain where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRenewDomainResponse' smart constructor.
data RenewDomainResponse = RenewDomainResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Identifier for tracking the progress of the request. To query the
    -- operation status, use
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail>.
    operationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RenewDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'renewDomainResponse_httpStatus' - The response's http status code.
--
-- 'operationId', 'renewDomainResponse_operationId' - Identifier for tracking the progress of the request. To query the
-- operation status, use
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail>.
newRenewDomainResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'operationId'
  Prelude.Text ->
  RenewDomainResponse
newRenewDomainResponse pHttpStatus_ pOperationId_ =
  RenewDomainResponse'
    { httpStatus = pHttpStatus_,
      operationId = pOperationId_
    }

-- | The response's http status code.
renewDomainResponse_httpStatus :: Lens.Lens' RenewDomainResponse Prelude.Int
renewDomainResponse_httpStatus = Lens.lens (\RenewDomainResponse' {httpStatus} -> httpStatus) (\s@RenewDomainResponse' {} a -> s {httpStatus = a} :: RenewDomainResponse)

-- | Identifier for tracking the progress of the request. To query the
-- operation status, use
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail>.
renewDomainResponse_operationId :: Lens.Lens' RenewDomainResponse Prelude.Text
renewDomainResponse_operationId = Lens.lens (\RenewDomainResponse' {operationId} -> operationId) (\s@RenewDomainResponse' {} a -> s {operationId = a} :: RenewDomainResponse)

instance Prelude.NFData RenewDomainResponse
