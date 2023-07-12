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
-- Module      : Amazonka.Route53Domains.AssociateDelegationSignerToDomain
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a delegation signer (DS) record in the registry zone for this
-- domain name.
--
-- Note that creating DS record at the registry impacts DNSSEC validation
-- of your DNS records. This action may render your domain name unavailable
-- on the internet if the steps are completed in the wrong order, or with
-- incorrect timing. For more information about DNSSEC signing, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-configuring-dnssec.html Configuring DNSSEC signing>
-- in the /Route 53 developer guide/.
module Amazonka.Route53Domains.AssociateDelegationSignerToDomain
  ( -- * Creating a Request
    AssociateDelegationSignerToDomain (..),
    newAssociateDelegationSignerToDomain,

    -- * Request Lenses
    associateDelegationSignerToDomain_domainName,
    associateDelegationSignerToDomain_signingAttributes,

    -- * Destructuring the Response
    AssociateDelegationSignerToDomainResponse (..),
    newAssociateDelegationSignerToDomainResponse,

    -- * Response Lenses
    associateDelegationSignerToDomainResponse_operationId,
    associateDelegationSignerToDomainResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Domains.Types

-- | /See:/ 'newAssociateDelegationSignerToDomain' smart constructor.
data AssociateDelegationSignerToDomain = AssociateDelegationSignerToDomain'
  { -- | The name of the domain.
    domainName :: Prelude.Text,
    -- | The information about a key, including the algorithm, public key-value,
    -- and flags.
    signingAttributes :: DnssecSigningAttributes
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateDelegationSignerToDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'associateDelegationSignerToDomain_domainName' - The name of the domain.
--
-- 'signingAttributes', 'associateDelegationSignerToDomain_signingAttributes' - The information about a key, including the algorithm, public key-value,
-- and flags.
newAssociateDelegationSignerToDomain ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'signingAttributes'
  DnssecSigningAttributes ->
  AssociateDelegationSignerToDomain
newAssociateDelegationSignerToDomain
  pDomainName_
  pSigningAttributes_ =
    AssociateDelegationSignerToDomain'
      { domainName =
          pDomainName_,
        signingAttributes = pSigningAttributes_
      }

-- | The name of the domain.
associateDelegationSignerToDomain_domainName :: Lens.Lens' AssociateDelegationSignerToDomain Prelude.Text
associateDelegationSignerToDomain_domainName = Lens.lens (\AssociateDelegationSignerToDomain' {domainName} -> domainName) (\s@AssociateDelegationSignerToDomain' {} a -> s {domainName = a} :: AssociateDelegationSignerToDomain)

-- | The information about a key, including the algorithm, public key-value,
-- and flags.
associateDelegationSignerToDomain_signingAttributes :: Lens.Lens' AssociateDelegationSignerToDomain DnssecSigningAttributes
associateDelegationSignerToDomain_signingAttributes = Lens.lens (\AssociateDelegationSignerToDomain' {signingAttributes} -> signingAttributes) (\s@AssociateDelegationSignerToDomain' {} a -> s {signingAttributes = a} :: AssociateDelegationSignerToDomain)

instance
  Core.AWSRequest
    AssociateDelegationSignerToDomain
  where
  type
    AWSResponse AssociateDelegationSignerToDomain =
      AssociateDelegationSignerToDomainResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateDelegationSignerToDomainResponse'
            Prelude.<$> (x Data..?> "OperationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AssociateDelegationSignerToDomain
  where
  hashWithSalt
    _salt
    AssociateDelegationSignerToDomain' {..} =
      _salt
        `Prelude.hashWithSalt` domainName
        `Prelude.hashWithSalt` signingAttributes

instance
  Prelude.NFData
    AssociateDelegationSignerToDomain
  where
  rnf AssociateDelegationSignerToDomain' {..} =
    Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf signingAttributes

instance
  Data.ToHeaders
    AssociateDelegationSignerToDomain
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Domains_v20140515.AssociateDelegationSignerToDomain" ::
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
    AssociateDelegationSignerToDomain
  where
  toJSON AssociateDelegationSignerToDomain' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DomainName" Data..= domainName),
            Prelude.Just
              ("SigningAttributes" Data..= signingAttributes)
          ]
      )

instance
  Data.ToPath
    AssociateDelegationSignerToDomain
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    AssociateDelegationSignerToDomain
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateDelegationSignerToDomainResponse' smart constructor.
data AssociateDelegationSignerToDomainResponse = AssociateDelegationSignerToDomainResponse'
  { -- | The identifier for tracking the progress of the request. To query the
    -- operation status, use
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail>.
    operationId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateDelegationSignerToDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operationId', 'associateDelegationSignerToDomainResponse_operationId' - The identifier for tracking the progress of the request. To query the
-- operation status, use
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail>.
--
-- 'httpStatus', 'associateDelegationSignerToDomainResponse_httpStatus' - The response's http status code.
newAssociateDelegationSignerToDomainResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateDelegationSignerToDomainResponse
newAssociateDelegationSignerToDomainResponse
  pHttpStatus_ =
    AssociateDelegationSignerToDomainResponse'
      { operationId =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The identifier for tracking the progress of the request. To query the
-- operation status, use
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail>.
associateDelegationSignerToDomainResponse_operationId :: Lens.Lens' AssociateDelegationSignerToDomainResponse (Prelude.Maybe Prelude.Text)
associateDelegationSignerToDomainResponse_operationId = Lens.lens (\AssociateDelegationSignerToDomainResponse' {operationId} -> operationId) (\s@AssociateDelegationSignerToDomainResponse' {} a -> s {operationId = a} :: AssociateDelegationSignerToDomainResponse)

-- | The response's http status code.
associateDelegationSignerToDomainResponse_httpStatus :: Lens.Lens' AssociateDelegationSignerToDomainResponse Prelude.Int
associateDelegationSignerToDomainResponse_httpStatus = Lens.lens (\AssociateDelegationSignerToDomainResponse' {httpStatus} -> httpStatus) (\s@AssociateDelegationSignerToDomainResponse' {} a -> s {httpStatus = a} :: AssociateDelegationSignerToDomainResponse)

instance
  Prelude.NFData
    AssociateDelegationSignerToDomainResponse
  where
  rnf AssociateDelegationSignerToDomainResponse' {..} =
    Prelude.rnf operationId
      `Prelude.seq` Prelude.rnf httpStatus
