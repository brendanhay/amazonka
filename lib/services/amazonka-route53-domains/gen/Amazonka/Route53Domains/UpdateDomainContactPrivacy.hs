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
-- Module      : Amazonka.Route53Domains.UpdateDomainContactPrivacy
-- Copyright   : (c) 2013-2023 Brendan Hay
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
-- You must specify the same privacy setting for the administrative,
-- registrant, and technical contacts.
--
-- This operation affects only the contact information for the specified
-- contact type (administrative, registrant, or technical). If the request
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
module Amazonka.Route53Domains.UpdateDomainContactPrivacy
  ( -- * Creating a Request
    UpdateDomainContactPrivacy (..),
    newUpdateDomainContactPrivacy,

    -- * Request Lenses
    updateDomainContactPrivacy_adminPrivacy,
    updateDomainContactPrivacy_registrantPrivacy,
    updateDomainContactPrivacy_techPrivacy,
    updateDomainContactPrivacy_domainName,

    -- * Destructuring the Response
    UpdateDomainContactPrivacyResponse (..),
    newUpdateDomainContactPrivacyResponse,

    -- * Response Lenses
    updateDomainContactPrivacyResponse_operationId,
    updateDomainContactPrivacyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Domains.Types

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
    --
    -- You must specify the same privacy setting for the administrative,
    -- registrant, and technical contacts.
    adminPrivacy :: Prelude.Maybe Prelude.Bool,
    -- | Whether you want to conceal contact information from WHOIS queries. If
    -- you specify @true@, WHOIS (\"who is\") queries return contact
    -- information either for Amazon Registrar (for .com, .net, and .org
    -- domains) or for our registrar associate, Gandi (for all other TLDs). If
    -- you specify @false@, WHOIS queries return the information that you
    -- entered for the registrant contact (domain owner).
    --
    -- You must specify the same privacy setting for the administrative,
    -- registrant, and technical contacts.
    registrantPrivacy :: Prelude.Maybe Prelude.Bool,
    -- | Whether you want to conceal contact information from WHOIS queries. If
    -- you specify @true@, WHOIS (\"who is\") queries return contact
    -- information either for Amazon Registrar (for .com, .net, and .org
    -- domains) or for our registrar associate, Gandi (for all other TLDs). If
    -- you specify @false@, WHOIS queries return the information that you
    -- entered for the technical contact.
    --
    -- You must specify the same privacy setting for the administrative,
    -- registrant, and technical contacts.
    techPrivacy :: Prelude.Maybe Prelude.Bool,
    -- | The name of the domain that you want to update the privacy setting for.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- You must specify the same privacy setting for the administrative,
-- registrant, and technical contacts.
--
-- 'registrantPrivacy', 'updateDomainContactPrivacy_registrantPrivacy' - Whether you want to conceal contact information from WHOIS queries. If
-- you specify @true@, WHOIS (\"who is\") queries return contact
-- information either for Amazon Registrar (for .com, .net, and .org
-- domains) or for our registrar associate, Gandi (for all other TLDs). If
-- you specify @false@, WHOIS queries return the information that you
-- entered for the registrant contact (domain owner).
--
-- You must specify the same privacy setting for the administrative,
-- registrant, and technical contacts.
--
-- 'techPrivacy', 'updateDomainContactPrivacy_techPrivacy' - Whether you want to conceal contact information from WHOIS queries. If
-- you specify @true@, WHOIS (\"who is\") queries return contact
-- information either for Amazon Registrar (for .com, .net, and .org
-- domains) or for our registrar associate, Gandi (for all other TLDs). If
-- you specify @false@, WHOIS queries return the information that you
-- entered for the technical contact.
--
-- You must specify the same privacy setting for the administrative,
-- registrant, and technical contacts.
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
      registrantPrivacy = Prelude.Nothing,
      techPrivacy = Prelude.Nothing,
      domainName = pDomainName_
    }

-- | Whether you want to conceal contact information from WHOIS queries. If
-- you specify @true@, WHOIS (\"who is\") queries return contact
-- information either for Amazon Registrar (for .com, .net, and .org
-- domains) or for our registrar associate, Gandi (for all other TLDs). If
-- you specify @false@, WHOIS queries return the information that you
-- entered for the admin contact.
--
-- You must specify the same privacy setting for the administrative,
-- registrant, and technical contacts.
updateDomainContactPrivacy_adminPrivacy :: Lens.Lens' UpdateDomainContactPrivacy (Prelude.Maybe Prelude.Bool)
updateDomainContactPrivacy_adminPrivacy = Lens.lens (\UpdateDomainContactPrivacy' {adminPrivacy} -> adminPrivacy) (\s@UpdateDomainContactPrivacy' {} a -> s {adminPrivacy = a} :: UpdateDomainContactPrivacy)

-- | Whether you want to conceal contact information from WHOIS queries. If
-- you specify @true@, WHOIS (\"who is\") queries return contact
-- information either for Amazon Registrar (for .com, .net, and .org
-- domains) or for our registrar associate, Gandi (for all other TLDs). If
-- you specify @false@, WHOIS queries return the information that you
-- entered for the registrant contact (domain owner).
--
-- You must specify the same privacy setting for the administrative,
-- registrant, and technical contacts.
updateDomainContactPrivacy_registrantPrivacy :: Lens.Lens' UpdateDomainContactPrivacy (Prelude.Maybe Prelude.Bool)
updateDomainContactPrivacy_registrantPrivacy = Lens.lens (\UpdateDomainContactPrivacy' {registrantPrivacy} -> registrantPrivacy) (\s@UpdateDomainContactPrivacy' {} a -> s {registrantPrivacy = a} :: UpdateDomainContactPrivacy)

-- | Whether you want to conceal contact information from WHOIS queries. If
-- you specify @true@, WHOIS (\"who is\") queries return contact
-- information either for Amazon Registrar (for .com, .net, and .org
-- domains) or for our registrar associate, Gandi (for all other TLDs). If
-- you specify @false@, WHOIS queries return the information that you
-- entered for the technical contact.
--
-- You must specify the same privacy setting for the administrative,
-- registrant, and technical contacts.
updateDomainContactPrivacy_techPrivacy :: Lens.Lens' UpdateDomainContactPrivacy (Prelude.Maybe Prelude.Bool)
updateDomainContactPrivacy_techPrivacy = Lens.lens (\UpdateDomainContactPrivacy' {techPrivacy} -> techPrivacy) (\s@UpdateDomainContactPrivacy' {} a -> s {techPrivacy = a} :: UpdateDomainContactPrivacy)

-- | The name of the domain that you want to update the privacy setting for.
updateDomainContactPrivacy_domainName :: Lens.Lens' UpdateDomainContactPrivacy Prelude.Text
updateDomainContactPrivacy_domainName = Lens.lens (\UpdateDomainContactPrivacy' {domainName} -> domainName) (\s@UpdateDomainContactPrivacy' {} a -> s {domainName = a} :: UpdateDomainContactPrivacy)

instance Core.AWSRequest UpdateDomainContactPrivacy where
  type
    AWSResponse UpdateDomainContactPrivacy =
      UpdateDomainContactPrivacyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDomainContactPrivacyResponse'
            Prelude.<$> (x Data..?> "OperationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDomainContactPrivacy where
  hashWithSalt _salt UpdateDomainContactPrivacy' {..} =
    _salt
      `Prelude.hashWithSalt` adminPrivacy
      `Prelude.hashWithSalt` registrantPrivacy
      `Prelude.hashWithSalt` techPrivacy
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData UpdateDomainContactPrivacy where
  rnf UpdateDomainContactPrivacy' {..} =
    Prelude.rnf adminPrivacy
      `Prelude.seq` Prelude.rnf registrantPrivacy
      `Prelude.seq` Prelude.rnf techPrivacy
      `Prelude.seq` Prelude.rnf domainName

instance Data.ToHeaders UpdateDomainContactPrivacy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Domains_v20140515.UpdateDomainContactPrivacy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateDomainContactPrivacy where
  toJSON UpdateDomainContactPrivacy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AdminPrivacy" Data..=) Prelude.<$> adminPrivacy,
            ("RegistrantPrivacy" Data..=)
              Prelude.<$> registrantPrivacy,
            ("TechPrivacy" Data..=) Prelude.<$> techPrivacy,
            Prelude.Just ("DomainName" Data..= domainName)
          ]
      )

instance Data.ToPath UpdateDomainContactPrivacy where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateDomainContactPrivacy where
  toQuery = Prelude.const Prelude.mempty

-- | The UpdateDomainContactPrivacy response includes the following element.
--
-- /See:/ 'newUpdateDomainContactPrivacyResponse' smart constructor.
data UpdateDomainContactPrivacyResponse = UpdateDomainContactPrivacyResponse'
  { -- | Identifier for tracking the progress of the request. To use this ID to
    -- query the operation status, use GetOperationDetail.
    operationId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDomainContactPrivacyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operationId', 'updateDomainContactPrivacyResponse_operationId' - Identifier for tracking the progress of the request. To use this ID to
-- query the operation status, use GetOperationDetail.
--
-- 'httpStatus', 'updateDomainContactPrivacyResponse_httpStatus' - The response's http status code.
newUpdateDomainContactPrivacyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateDomainContactPrivacyResponse
newUpdateDomainContactPrivacyResponse pHttpStatus_ =
  UpdateDomainContactPrivacyResponse'
    { operationId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Identifier for tracking the progress of the request. To use this ID to
-- query the operation status, use GetOperationDetail.
updateDomainContactPrivacyResponse_operationId :: Lens.Lens' UpdateDomainContactPrivacyResponse (Prelude.Maybe Prelude.Text)
updateDomainContactPrivacyResponse_operationId = Lens.lens (\UpdateDomainContactPrivacyResponse' {operationId} -> operationId) (\s@UpdateDomainContactPrivacyResponse' {} a -> s {operationId = a} :: UpdateDomainContactPrivacyResponse)

-- | The response's http status code.
updateDomainContactPrivacyResponse_httpStatus :: Lens.Lens' UpdateDomainContactPrivacyResponse Prelude.Int
updateDomainContactPrivacyResponse_httpStatus = Lens.lens (\UpdateDomainContactPrivacyResponse' {httpStatus} -> httpStatus) (\s@UpdateDomainContactPrivacyResponse' {} a -> s {httpStatus = a} :: UpdateDomainContactPrivacyResponse)

instance
  Prelude.NFData
    UpdateDomainContactPrivacyResponse
  where
  rnf UpdateDomainContactPrivacyResponse' {..} =
    Prelude.rnf operationId
      `Prelude.seq` Prelude.rnf httpStatus
