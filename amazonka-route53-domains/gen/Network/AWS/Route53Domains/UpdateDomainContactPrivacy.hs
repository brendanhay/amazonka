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
-- Module      : Network.AWS.Route53Domains.UpdateDomainContactPrivacy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation updates the specified domain contact\'s privacy setting.
-- When privacy protection is enabled, contact information such as email
-- address is replaced either with contact information for Amazon Registrar
-- (for .com, .net, and .org domains) or with contact information for our
-- registrar associate, Gandi.
--
-- This operation affects only the contact information for the specified
-- contact type (registrant, administrator, or tech). If the request
-- succeeds, Amazon Route 53 returns an operation ID that you can use with
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail>
-- to track the progress and completion of the action. If the request
-- doesn\'t complete successfully, the domain registrant will be notified
-- by email.
--
-- By disabling the privacy service via API, you consent to the publication
-- of the contact information provided for this domain via the public WHOIS
-- database. You certify that you are the registrant of this domain name
-- and have the authority to make this decision. You may withdraw your
-- consent at any time by enabling privacy protection using either
-- @UpdateDomainContactPrivacy@ or the Route 53 console. Enabling privacy
-- protection removes the contact information provided for this domain from
-- the WHOIS database. For more information on our privacy practices, see
-- <https://aws.amazon.com/privacy/>.
module Network.AWS.Route53Domains.UpdateDomainContactPrivacy
  ( -- * Creating a Request
    UpdateDomainContactPrivacy (..),
    newUpdateDomainContactPrivacy,

    -- * Request Lenses
    updateDomainContactPrivacy_adminPrivacy,
    updateDomainContactPrivacy_techPrivacy,
    updateDomainContactPrivacy_registrantPrivacy,
    updateDomainContactPrivacy_domainName,

    -- * Destructuring the Response
    UpdateDomainContactPrivacyResponse (..),
    newUpdateDomainContactPrivacyResponse,

    -- * Response Lenses
    updateDomainContactPrivacyResponse_httpStatus,
    updateDomainContactPrivacyResponse_operationId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53Domains.Types

-- | The UpdateDomainContactPrivacy request includes the following elements.
--
-- /See:/ 'newUpdateDomainContactPrivacy' smart constructor.
data UpdateDomainContactPrivacy = UpdateDomainContactPrivacy'
  { -- | Whether you want to conceal contact information from WHOIS queries. If
    -- you specify @true@, WHOIS (\"who is\") queries return contact
    -- information either for Amazon Registrar (for .com, .net, and .org
    -- domains) or for our registrar associate, Gandi (for all other TLDs). If
    -- you specify @false@, WHOIS queries return the information that you
    -- entered for the admin contact.
    adminPrivacy :: Prelude.Maybe Prelude.Bool,
    -- | Whether you want to conceal contact information from WHOIS queries. If
    -- you specify @true@, WHOIS (\"who is\") queries return contact
    -- information either for Amazon Registrar (for .com, .net, and .org
    -- domains) or for our registrar associate, Gandi (for all other TLDs). If
    -- you specify @false@, WHOIS queries return the information that you
    -- entered for the technical contact.
    techPrivacy :: Prelude.Maybe Prelude.Bool,
    -- | Whether you want to conceal contact information from WHOIS queries. If
    -- you specify @true@, WHOIS (\"who is\") queries return contact
    -- information either for Amazon Registrar (for .com, .net, and .org
    -- domains) or for our registrar associate, Gandi (for all other TLDs). If
    -- you specify @false@, WHOIS queries return the information that you
    -- entered for the registrant contact (domain owner).
    registrantPrivacy :: Prelude.Maybe Prelude.Bool,
    -- | The name of the domain that you want to update the privacy setting for.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateDomainContactPrivacy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adminPrivacy', 'updateDomainContactPrivacy_adminPrivacy' - Whether you want to conceal contact information from WHOIS queries. If
-- you specify @true@, WHOIS (\"who is\") queries return contact
-- information either for Amazon Registrar (for .com, .net, and .org
-- domains) or for our registrar associate, Gandi (for all other TLDs). If
-- you specify @false@, WHOIS queries return the information that you
-- entered for the admin contact.
--
-- 'techPrivacy', 'updateDomainContactPrivacy_techPrivacy' - Whether you want to conceal contact information from WHOIS queries. If
-- you specify @true@, WHOIS (\"who is\") queries return contact
-- information either for Amazon Registrar (for .com, .net, and .org
-- domains) or for our registrar associate, Gandi (for all other TLDs). If
-- you specify @false@, WHOIS queries return the information that you
-- entered for the technical contact.
--
-- 'registrantPrivacy', 'updateDomainContactPrivacy_registrantPrivacy' - Whether you want to conceal contact information from WHOIS queries. If
-- you specify @true@, WHOIS (\"who is\") queries return contact
-- information either for Amazon Registrar (for .com, .net, and .org
-- domains) or for our registrar associate, Gandi (for all other TLDs). If
-- you specify @false@, WHOIS queries return the information that you
-- entered for the registrant contact (domain owner).
--
-- 'domainName', 'updateDomainContactPrivacy_domainName' - The name of the domain that you want to update the privacy setting for.
newUpdateDomainContactPrivacy ::
  -- | 'domainName'
  Prelude.Text ->
  UpdateDomainContactPrivacy
newUpdateDomainContactPrivacy pDomainName_ =
  UpdateDomainContactPrivacy'
    { adminPrivacy =
        Prelude.Nothing,
      techPrivacy = Prelude.Nothing,
      registrantPrivacy = Prelude.Nothing,
      domainName = pDomainName_
    }

-- | Whether you want to conceal contact information from WHOIS queries. If
-- you specify @true@, WHOIS (\"who is\") queries return contact
-- information either for Amazon Registrar (for .com, .net, and .org
-- domains) or for our registrar associate, Gandi (for all other TLDs). If
-- you specify @false@, WHOIS queries return the information that you
-- entered for the admin contact.
updateDomainContactPrivacy_adminPrivacy :: Lens.Lens' UpdateDomainContactPrivacy (Prelude.Maybe Prelude.Bool)
updateDomainContactPrivacy_adminPrivacy = Lens.lens (\UpdateDomainContactPrivacy' {adminPrivacy} -> adminPrivacy) (\s@UpdateDomainContactPrivacy' {} a -> s {adminPrivacy = a} :: UpdateDomainContactPrivacy)

-- | Whether you want to conceal contact information from WHOIS queries. If
-- you specify @true@, WHOIS (\"who is\") queries return contact
-- information either for Amazon Registrar (for .com, .net, and .org
-- domains) or for our registrar associate, Gandi (for all other TLDs). If
-- you specify @false@, WHOIS queries return the information that you
-- entered for the technical contact.
updateDomainContactPrivacy_techPrivacy :: Lens.Lens' UpdateDomainContactPrivacy (Prelude.Maybe Prelude.Bool)
updateDomainContactPrivacy_techPrivacy = Lens.lens (\UpdateDomainContactPrivacy' {techPrivacy} -> techPrivacy) (\s@UpdateDomainContactPrivacy' {} a -> s {techPrivacy = a} :: UpdateDomainContactPrivacy)

-- | Whether you want to conceal contact information from WHOIS queries. If
-- you specify @true@, WHOIS (\"who is\") queries return contact
-- information either for Amazon Registrar (for .com, .net, and .org
-- domains) or for our registrar associate, Gandi (for all other TLDs). If
-- you specify @false@, WHOIS queries return the information that you
-- entered for the registrant contact (domain owner).
updateDomainContactPrivacy_registrantPrivacy :: Lens.Lens' UpdateDomainContactPrivacy (Prelude.Maybe Prelude.Bool)
updateDomainContactPrivacy_registrantPrivacy = Lens.lens (\UpdateDomainContactPrivacy' {registrantPrivacy} -> registrantPrivacy) (\s@UpdateDomainContactPrivacy' {} a -> s {registrantPrivacy = a} :: UpdateDomainContactPrivacy)

-- | The name of the domain that you want to update the privacy setting for.
updateDomainContactPrivacy_domainName :: Lens.Lens' UpdateDomainContactPrivacy Prelude.Text
updateDomainContactPrivacy_domainName = Lens.lens (\UpdateDomainContactPrivacy' {domainName} -> domainName) (\s@UpdateDomainContactPrivacy' {} a -> s {domainName = a} :: UpdateDomainContactPrivacy)

instance
  Prelude.AWSRequest
    UpdateDomainContactPrivacy
  where
  type
    Rs UpdateDomainContactPrivacy =
      UpdateDomainContactPrivacyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDomainContactPrivacyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "OperationId")
      )

instance Prelude.Hashable UpdateDomainContactPrivacy

instance Prelude.NFData UpdateDomainContactPrivacy

instance Prelude.ToHeaders UpdateDomainContactPrivacy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Route53Domains_v20140515.UpdateDomainContactPrivacy" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateDomainContactPrivacy where
  toJSON UpdateDomainContactPrivacy' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("AdminPrivacy" Prelude..=)
              Prelude.<$> adminPrivacy,
            ("TechPrivacy" Prelude..=) Prelude.<$> techPrivacy,
            ("RegistrantPrivacy" Prelude..=)
              Prelude.<$> registrantPrivacy,
            Prelude.Just ("DomainName" Prelude..= domainName)
          ]
      )

instance Prelude.ToPath UpdateDomainContactPrivacy where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateDomainContactPrivacy where
  toQuery = Prelude.const Prelude.mempty

-- | The UpdateDomainContactPrivacy response includes the following element.
--
-- /See:/ 'newUpdateDomainContactPrivacyResponse' smart constructor.
data UpdateDomainContactPrivacyResponse = UpdateDomainContactPrivacyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Identifier for tracking the progress of the request. To use this ID to
    -- query the operation status, use GetOperationDetail.
    operationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateDomainContactPrivacyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateDomainContactPrivacyResponse_httpStatus' - The response's http status code.
--
-- 'operationId', 'updateDomainContactPrivacyResponse_operationId' - Identifier for tracking the progress of the request. To use this ID to
-- query the operation status, use GetOperationDetail.
newUpdateDomainContactPrivacyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'operationId'
  Prelude.Text ->
  UpdateDomainContactPrivacyResponse
newUpdateDomainContactPrivacyResponse
  pHttpStatus_
  pOperationId_ =
    UpdateDomainContactPrivacyResponse'
      { httpStatus =
          pHttpStatus_,
        operationId = pOperationId_
      }

-- | The response's http status code.
updateDomainContactPrivacyResponse_httpStatus :: Lens.Lens' UpdateDomainContactPrivacyResponse Prelude.Int
updateDomainContactPrivacyResponse_httpStatus = Lens.lens (\UpdateDomainContactPrivacyResponse' {httpStatus} -> httpStatus) (\s@UpdateDomainContactPrivacyResponse' {} a -> s {httpStatus = a} :: UpdateDomainContactPrivacyResponse)

-- | Identifier for tracking the progress of the request. To use this ID to
-- query the operation status, use GetOperationDetail.
updateDomainContactPrivacyResponse_operationId :: Lens.Lens' UpdateDomainContactPrivacyResponse Prelude.Text
updateDomainContactPrivacyResponse_operationId = Lens.lens (\UpdateDomainContactPrivacyResponse' {operationId} -> operationId) (\s@UpdateDomainContactPrivacyResponse' {} a -> s {operationId = a} :: UpdateDomainContactPrivacyResponse)

instance
  Prelude.NFData
    UpdateDomainContactPrivacyResponse
