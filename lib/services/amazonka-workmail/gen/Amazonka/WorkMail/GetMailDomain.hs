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
-- Module      : Amazonka.WorkMail.GetMailDomain
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details for a mail domain, including domain records required to
-- configure your domain with recommended security.
module Amazonka.WorkMail.GetMailDomain
  ( -- * Creating a Request
    GetMailDomain (..),
    newGetMailDomain,

    -- * Request Lenses
    getMailDomain_organizationId,
    getMailDomain_domainName,

    -- * Destructuring the Response
    GetMailDomainResponse (..),
    newGetMailDomainResponse,

    -- * Response Lenses
    getMailDomainResponse_dkimVerificationStatus,
    getMailDomainResponse_isDefault,
    getMailDomainResponse_isTestDomain,
    getMailDomainResponse_ownershipVerificationStatus,
    getMailDomainResponse_records,
    getMailDomainResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkMail.Types

-- | /See:/ 'newGetMailDomain' smart constructor.
data GetMailDomain = GetMailDomain'
  { -- | The WorkMail organization for which the domain is retrieved.
    organizationId :: Prelude.Text,
    -- | The domain from which you want to retrieve details.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMailDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationId', 'getMailDomain_organizationId' - The WorkMail organization for which the domain is retrieved.
--
-- 'domainName', 'getMailDomain_domainName' - The domain from which you want to retrieve details.
newGetMailDomain ::
  -- | 'organizationId'
  Prelude.Text ->
  -- | 'domainName'
  Prelude.Text ->
  GetMailDomain
newGetMailDomain pOrganizationId_ pDomainName_ =
  GetMailDomain'
    { organizationId = pOrganizationId_,
      domainName = pDomainName_
    }

-- | The WorkMail organization for which the domain is retrieved.
getMailDomain_organizationId :: Lens.Lens' GetMailDomain Prelude.Text
getMailDomain_organizationId = Lens.lens (\GetMailDomain' {organizationId} -> organizationId) (\s@GetMailDomain' {} a -> s {organizationId = a} :: GetMailDomain)

-- | The domain from which you want to retrieve details.
getMailDomain_domainName :: Lens.Lens' GetMailDomain Prelude.Text
getMailDomain_domainName = Lens.lens (\GetMailDomain' {domainName} -> domainName) (\s@GetMailDomain' {} a -> s {domainName = a} :: GetMailDomain)

instance Core.AWSRequest GetMailDomain where
  type
    AWSResponse GetMailDomain =
      GetMailDomainResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMailDomainResponse'
            Prelude.<$> (x Data..?> "DkimVerificationStatus")
            Prelude.<*> (x Data..?> "IsDefault")
            Prelude.<*> (x Data..?> "IsTestDomain")
            Prelude.<*> (x Data..?> "OwnershipVerificationStatus")
            Prelude.<*> (x Data..?> "Records" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetMailDomain where
  hashWithSalt _salt GetMailDomain' {..} =
    _salt
      `Prelude.hashWithSalt` organizationId
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData GetMailDomain where
  rnf GetMailDomain' {..} =
    Prelude.rnf organizationId
      `Prelude.seq` Prelude.rnf domainName

instance Data.ToHeaders GetMailDomain where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkMailService.GetMailDomain" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetMailDomain where
  toJSON GetMailDomain' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("OrganizationId" Data..= organizationId),
            Prelude.Just ("DomainName" Data..= domainName)
          ]
      )

instance Data.ToPath GetMailDomain where
  toPath = Prelude.const "/"

instance Data.ToQuery GetMailDomain where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetMailDomainResponse' smart constructor.
data GetMailDomainResponse = GetMailDomainResponse'
  { -- | Indicates the status of a DKIM verification.
    dkimVerificationStatus :: Prelude.Maybe DnsRecordVerificationStatus,
    -- | Specifies whether the domain is the default domain for your
    -- organization.
    isDefault :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether the domain is a test domain provided by WorkMail, or a
    -- custom domain.
    isTestDomain :: Prelude.Maybe Prelude.Bool,
    -- | Indicates the status of the domain ownership verification.
    ownershipVerificationStatus :: Prelude.Maybe DnsRecordVerificationStatus,
    -- | A list of the DNS records that WorkMail recommends adding in your DNS
    -- provider for the best user experience. The records configure your domain
    -- with DMARC, SPF, DKIM, and direct incoming email traffic to SES. See
    -- admin guide for more details.
    records :: Prelude.Maybe [DnsRecord],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMailDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dkimVerificationStatus', 'getMailDomainResponse_dkimVerificationStatus' - Indicates the status of a DKIM verification.
--
-- 'isDefault', 'getMailDomainResponse_isDefault' - Specifies whether the domain is the default domain for your
-- organization.
--
-- 'isTestDomain', 'getMailDomainResponse_isTestDomain' - Specifies whether the domain is a test domain provided by WorkMail, or a
-- custom domain.
--
-- 'ownershipVerificationStatus', 'getMailDomainResponse_ownershipVerificationStatus' - Indicates the status of the domain ownership verification.
--
-- 'records', 'getMailDomainResponse_records' - A list of the DNS records that WorkMail recommends adding in your DNS
-- provider for the best user experience. The records configure your domain
-- with DMARC, SPF, DKIM, and direct incoming email traffic to SES. See
-- admin guide for more details.
--
-- 'httpStatus', 'getMailDomainResponse_httpStatus' - The response's http status code.
newGetMailDomainResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetMailDomainResponse
newGetMailDomainResponse pHttpStatus_ =
  GetMailDomainResponse'
    { dkimVerificationStatus =
        Prelude.Nothing,
      isDefault = Prelude.Nothing,
      isTestDomain = Prelude.Nothing,
      ownershipVerificationStatus = Prelude.Nothing,
      records = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Indicates the status of a DKIM verification.
getMailDomainResponse_dkimVerificationStatus :: Lens.Lens' GetMailDomainResponse (Prelude.Maybe DnsRecordVerificationStatus)
getMailDomainResponse_dkimVerificationStatus = Lens.lens (\GetMailDomainResponse' {dkimVerificationStatus} -> dkimVerificationStatus) (\s@GetMailDomainResponse' {} a -> s {dkimVerificationStatus = a} :: GetMailDomainResponse)

-- | Specifies whether the domain is the default domain for your
-- organization.
getMailDomainResponse_isDefault :: Lens.Lens' GetMailDomainResponse (Prelude.Maybe Prelude.Bool)
getMailDomainResponse_isDefault = Lens.lens (\GetMailDomainResponse' {isDefault} -> isDefault) (\s@GetMailDomainResponse' {} a -> s {isDefault = a} :: GetMailDomainResponse)

-- | Specifies whether the domain is a test domain provided by WorkMail, or a
-- custom domain.
getMailDomainResponse_isTestDomain :: Lens.Lens' GetMailDomainResponse (Prelude.Maybe Prelude.Bool)
getMailDomainResponse_isTestDomain = Lens.lens (\GetMailDomainResponse' {isTestDomain} -> isTestDomain) (\s@GetMailDomainResponse' {} a -> s {isTestDomain = a} :: GetMailDomainResponse)

-- | Indicates the status of the domain ownership verification.
getMailDomainResponse_ownershipVerificationStatus :: Lens.Lens' GetMailDomainResponse (Prelude.Maybe DnsRecordVerificationStatus)
getMailDomainResponse_ownershipVerificationStatus = Lens.lens (\GetMailDomainResponse' {ownershipVerificationStatus} -> ownershipVerificationStatus) (\s@GetMailDomainResponse' {} a -> s {ownershipVerificationStatus = a} :: GetMailDomainResponse)

-- | A list of the DNS records that WorkMail recommends adding in your DNS
-- provider for the best user experience. The records configure your domain
-- with DMARC, SPF, DKIM, and direct incoming email traffic to SES. See
-- admin guide for more details.
getMailDomainResponse_records :: Lens.Lens' GetMailDomainResponse (Prelude.Maybe [DnsRecord])
getMailDomainResponse_records = Lens.lens (\GetMailDomainResponse' {records} -> records) (\s@GetMailDomainResponse' {} a -> s {records = a} :: GetMailDomainResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getMailDomainResponse_httpStatus :: Lens.Lens' GetMailDomainResponse Prelude.Int
getMailDomainResponse_httpStatus = Lens.lens (\GetMailDomainResponse' {httpStatus} -> httpStatus) (\s@GetMailDomainResponse' {} a -> s {httpStatus = a} :: GetMailDomainResponse)

instance Prelude.NFData GetMailDomainResponse where
  rnf GetMailDomainResponse' {..} =
    Prelude.rnf dkimVerificationStatus
      `Prelude.seq` Prelude.rnf isDefault
      `Prelude.seq` Prelude.rnf isTestDomain
      `Prelude.seq` Prelude.rnf ownershipVerificationStatus
      `Prelude.seq` Prelude.rnf records
      `Prelude.seq` Prelude.rnf httpStatus
