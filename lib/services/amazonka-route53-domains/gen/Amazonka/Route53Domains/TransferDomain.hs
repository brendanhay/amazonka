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
-- Module      : Amazonka.Route53Domains.TransferDomain
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Transfers a domain from another registrar to Amazon Route 53. When the
-- transfer is complete, the domain is registered either with Amazon
-- Registrar (for .com, .net, and .org domains) or with our registrar
-- associate, Gandi (for all other TLDs).
--
-- For more information about transferring domains, see the following
-- topics:
--
-- -   For transfer requirements, a detailed procedure, and information
--     about viewing the status of a domain that you\'re transferring to
--     Route 53, see
--     <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/domain-transfer-to-route-53.html Transferring Registration for a Domain to Amazon Route 53>
--     in the /Amazon Route 53 Developer Guide/.
--
-- -   For information about how to transfer a domain from one Amazon Web
--     Services account to another, see
--     <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_TransferDomainToAnotherAwsAccount.html TransferDomainToAnotherAwsAccount>.
--
-- -   For information about how to transfer a domain to another domain
--     registrar, see
--     <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/domain-transfer-from-route-53.html Transferring a Domain from Amazon Route 53 to Another Registrar>
--     in the /Amazon Route 53 Developer Guide/.
--
-- If the registrar for your domain is also the DNS service provider for
-- the domain, we highly recommend that you transfer your DNS service to
-- Route 53 or to another DNS service provider before you transfer your
-- registration. Some registrars provide free DNS service when you purchase
-- a domain registration. When you transfer the registration, the previous
-- registrar will not renew your domain registration and could end your DNS
-- service at any time.
--
-- If the registrar for your domain is also the DNS service provider for
-- the domain and you don\'t transfer DNS service to another provider, your
-- website, email, and the web applications associated with the domain
-- might become unavailable.
--
-- If the transfer is successful, this method returns an operation ID that
-- you can use to track the progress and completion of the action. If the
-- transfer doesn\'t complete successfully, the domain registrant will be
-- notified by email.
module Amazonka.Route53Domains.TransferDomain
  ( -- * Creating a Request
    TransferDomain (..),
    newTransferDomain,

    -- * Request Lenses
    transferDomain_authCode,
    transferDomain_autoRenew,
    transferDomain_idnLangCode,
    transferDomain_nameservers,
    transferDomain_privacyProtectAdminContact,
    transferDomain_privacyProtectRegistrantContact,
    transferDomain_privacyProtectTechContact,
    transferDomain_domainName,
    transferDomain_durationInYears,
    transferDomain_adminContact,
    transferDomain_registrantContact,
    transferDomain_techContact,

    -- * Destructuring the Response
    TransferDomainResponse (..),
    newTransferDomainResponse,

    -- * Response Lenses
    transferDomainResponse_operationId,
    transferDomainResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Domains.Types

-- | The TransferDomain request includes the following elements.
--
-- /See:/ 'newTransferDomain' smart constructor.
data TransferDomain = TransferDomain'
  { -- | The authorization code for the domain. You get this value from the
    -- current registrar.
    authCode :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Indicates whether the domain will be automatically renewed (true) or not
    -- (false). Auto renewal only takes effect after the account is charged.
    --
    -- Default: true
    autoRenew :: Prelude.Maybe Prelude.Bool,
    -- | Reserved for future use.
    idnLangCode :: Prelude.Maybe Prelude.Text,
    -- | Contains details for the host and glue IP addresses.
    nameservers :: Prelude.Maybe [Nameserver],
    -- | Whether you want to conceal contact information from WHOIS queries. If
    -- you specify @true@, WHOIS (\"who is\") queries return contact
    -- information either for Amazon Registrar (for .com, .net, and .org
    -- domains) or for our registrar associate, Gandi (for all other TLDs). If
    -- you specify @false@, WHOIS queries return the information that you
    -- entered for the admin contact.
    --
    -- You must specify the same privacy setting for the administrative,
    -- registrant, and technical contacts.
    --
    -- Default: @true@
    privacyProtectAdminContact :: Prelude.Maybe Prelude.Bool,
    -- | Whether you want to conceal contact information from WHOIS queries. If
    -- you specify @true@, WHOIS (\"who is\") queries return contact
    -- information either for Amazon Registrar (for .com, .net, and .org
    -- domains) or for our registrar associate, Gandi (for all other TLDs). If
    -- you specify @false@, WHOIS queries return the information that you
    -- entered for the registrant contact (domain owner).
    --
    -- You must specify the same privacy setting for the administrative,
    -- registrant, and technical contacts.
    --
    -- Default: @true@
    privacyProtectRegistrantContact :: Prelude.Maybe Prelude.Bool,
    -- | Whether you want to conceal contact information from WHOIS queries. If
    -- you specify @true@, WHOIS (\"who is\") queries return contact
    -- information either for Amazon Registrar (for .com, .net, and .org
    -- domains) or for our registrar associate, Gandi (for all other TLDs). If
    -- you specify @false@, WHOIS queries return the information that you
    -- entered for the technical contact.
    --
    -- You must specify the same privacy setting for the administrative,
    -- registrant, and technical contacts.
    --
    -- Default: @true@
    privacyProtectTechContact :: Prelude.Maybe Prelude.Bool,
    -- | The name of the domain that you want to transfer to Route 53. The
    -- top-level domain (TLD), such as .com, must be a TLD that Route 53
    -- supports. For a list of supported TLDs, see
    -- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53>
    -- in the /Amazon Route 53 Developer Guide/.
    --
    -- The domain name can contain only the following characters:
    --
    -- -   Letters a through z. Domain names are not case sensitive.
    --
    -- -   Numbers 0 through 9.
    --
    -- -   Hyphen (-). You can\'t specify a hyphen at the beginning or end of a
    --     label.
    --
    -- -   Period (.) to separate the labels in the name, such as the @.@ in
    --     @example.com@.
    domainName :: Prelude.Text,
    -- | The number of years that you want to register the domain for. Domains
    -- are registered for a minimum of one year. The maximum period depends on
    -- the top-level domain.
    --
    -- Default: 1
    durationInYears :: Prelude.Natural,
    -- | Provides detailed contact information.
    adminContact :: Data.Sensitive ContactDetail,
    -- | Provides detailed contact information.
    registrantContact :: Data.Sensitive ContactDetail,
    -- | Provides detailed contact information.
    techContact :: Data.Sensitive ContactDetail
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransferDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authCode', 'transferDomain_authCode' - The authorization code for the domain. You get this value from the
-- current registrar.
--
-- 'autoRenew', 'transferDomain_autoRenew' - Indicates whether the domain will be automatically renewed (true) or not
-- (false). Auto renewal only takes effect after the account is charged.
--
-- Default: true
--
-- 'idnLangCode', 'transferDomain_idnLangCode' - Reserved for future use.
--
-- 'nameservers', 'transferDomain_nameservers' - Contains details for the host and glue IP addresses.
--
-- 'privacyProtectAdminContact', 'transferDomain_privacyProtectAdminContact' - Whether you want to conceal contact information from WHOIS queries. If
-- you specify @true@, WHOIS (\"who is\") queries return contact
-- information either for Amazon Registrar (for .com, .net, and .org
-- domains) or for our registrar associate, Gandi (for all other TLDs). If
-- you specify @false@, WHOIS queries return the information that you
-- entered for the admin contact.
--
-- You must specify the same privacy setting for the administrative,
-- registrant, and technical contacts.
--
-- Default: @true@
--
-- 'privacyProtectRegistrantContact', 'transferDomain_privacyProtectRegistrantContact' - Whether you want to conceal contact information from WHOIS queries. If
-- you specify @true@, WHOIS (\"who is\") queries return contact
-- information either for Amazon Registrar (for .com, .net, and .org
-- domains) or for our registrar associate, Gandi (for all other TLDs). If
-- you specify @false@, WHOIS queries return the information that you
-- entered for the registrant contact (domain owner).
--
-- You must specify the same privacy setting for the administrative,
-- registrant, and technical contacts.
--
-- Default: @true@
--
-- 'privacyProtectTechContact', 'transferDomain_privacyProtectTechContact' - Whether you want to conceal contact information from WHOIS queries. If
-- you specify @true@, WHOIS (\"who is\") queries return contact
-- information either for Amazon Registrar (for .com, .net, and .org
-- domains) or for our registrar associate, Gandi (for all other TLDs). If
-- you specify @false@, WHOIS queries return the information that you
-- entered for the technical contact.
--
-- You must specify the same privacy setting for the administrative,
-- registrant, and technical contacts.
--
-- Default: @true@
--
-- 'domainName', 'transferDomain_domainName' - The name of the domain that you want to transfer to Route 53. The
-- top-level domain (TLD), such as .com, must be a TLD that Route 53
-- supports. For a list of supported TLDs, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53>
-- in the /Amazon Route 53 Developer Guide/.
--
-- The domain name can contain only the following characters:
--
-- -   Letters a through z. Domain names are not case sensitive.
--
-- -   Numbers 0 through 9.
--
-- -   Hyphen (-). You can\'t specify a hyphen at the beginning or end of a
--     label.
--
-- -   Period (.) to separate the labels in the name, such as the @.@ in
--     @example.com@.
--
-- 'durationInYears', 'transferDomain_durationInYears' - The number of years that you want to register the domain for. Domains
-- are registered for a minimum of one year. The maximum period depends on
-- the top-level domain.
--
-- Default: 1
--
-- 'adminContact', 'transferDomain_adminContact' - Provides detailed contact information.
--
-- 'registrantContact', 'transferDomain_registrantContact' - Provides detailed contact information.
--
-- 'techContact', 'transferDomain_techContact' - Provides detailed contact information.
newTransferDomain ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'durationInYears'
  Prelude.Natural ->
  -- | 'adminContact'
  ContactDetail ->
  -- | 'registrantContact'
  ContactDetail ->
  -- | 'techContact'
  ContactDetail ->
  TransferDomain
newTransferDomain
  pDomainName_
  pDurationInYears_
  pAdminContact_
  pRegistrantContact_
  pTechContact_ =
    TransferDomain'
      { authCode = Prelude.Nothing,
        autoRenew = Prelude.Nothing,
        idnLangCode = Prelude.Nothing,
        nameservers = Prelude.Nothing,
        privacyProtectAdminContact = Prelude.Nothing,
        privacyProtectRegistrantContact = Prelude.Nothing,
        privacyProtectTechContact = Prelude.Nothing,
        domainName = pDomainName_,
        durationInYears = pDurationInYears_,
        adminContact = Data._Sensitive Lens.# pAdminContact_,
        registrantContact =
          Data._Sensitive Lens.# pRegistrantContact_,
        techContact = Data._Sensitive Lens.# pTechContact_
      }

-- | The authorization code for the domain. You get this value from the
-- current registrar.
transferDomain_authCode :: Lens.Lens' TransferDomain (Prelude.Maybe Prelude.Text)
transferDomain_authCode = Lens.lens (\TransferDomain' {authCode} -> authCode) (\s@TransferDomain' {} a -> s {authCode = a} :: TransferDomain) Prelude.. Lens.mapping Data._Sensitive

-- | Indicates whether the domain will be automatically renewed (true) or not
-- (false). Auto renewal only takes effect after the account is charged.
--
-- Default: true
transferDomain_autoRenew :: Lens.Lens' TransferDomain (Prelude.Maybe Prelude.Bool)
transferDomain_autoRenew = Lens.lens (\TransferDomain' {autoRenew} -> autoRenew) (\s@TransferDomain' {} a -> s {autoRenew = a} :: TransferDomain)

-- | Reserved for future use.
transferDomain_idnLangCode :: Lens.Lens' TransferDomain (Prelude.Maybe Prelude.Text)
transferDomain_idnLangCode = Lens.lens (\TransferDomain' {idnLangCode} -> idnLangCode) (\s@TransferDomain' {} a -> s {idnLangCode = a} :: TransferDomain)

-- | Contains details for the host and glue IP addresses.
transferDomain_nameservers :: Lens.Lens' TransferDomain (Prelude.Maybe [Nameserver])
transferDomain_nameservers = Lens.lens (\TransferDomain' {nameservers} -> nameservers) (\s@TransferDomain' {} a -> s {nameservers = a} :: TransferDomain) Prelude.. Lens.mapping Lens.coerced

-- | Whether you want to conceal contact information from WHOIS queries. If
-- you specify @true@, WHOIS (\"who is\") queries return contact
-- information either for Amazon Registrar (for .com, .net, and .org
-- domains) or for our registrar associate, Gandi (for all other TLDs). If
-- you specify @false@, WHOIS queries return the information that you
-- entered for the admin contact.
--
-- You must specify the same privacy setting for the administrative,
-- registrant, and technical contacts.
--
-- Default: @true@
transferDomain_privacyProtectAdminContact :: Lens.Lens' TransferDomain (Prelude.Maybe Prelude.Bool)
transferDomain_privacyProtectAdminContact = Lens.lens (\TransferDomain' {privacyProtectAdminContact} -> privacyProtectAdminContact) (\s@TransferDomain' {} a -> s {privacyProtectAdminContact = a} :: TransferDomain)

-- | Whether you want to conceal contact information from WHOIS queries. If
-- you specify @true@, WHOIS (\"who is\") queries return contact
-- information either for Amazon Registrar (for .com, .net, and .org
-- domains) or for our registrar associate, Gandi (for all other TLDs). If
-- you specify @false@, WHOIS queries return the information that you
-- entered for the registrant contact (domain owner).
--
-- You must specify the same privacy setting for the administrative,
-- registrant, and technical contacts.
--
-- Default: @true@
transferDomain_privacyProtectRegistrantContact :: Lens.Lens' TransferDomain (Prelude.Maybe Prelude.Bool)
transferDomain_privacyProtectRegistrantContact = Lens.lens (\TransferDomain' {privacyProtectRegistrantContact} -> privacyProtectRegistrantContact) (\s@TransferDomain' {} a -> s {privacyProtectRegistrantContact = a} :: TransferDomain)

-- | Whether you want to conceal contact information from WHOIS queries. If
-- you specify @true@, WHOIS (\"who is\") queries return contact
-- information either for Amazon Registrar (for .com, .net, and .org
-- domains) or for our registrar associate, Gandi (for all other TLDs). If
-- you specify @false@, WHOIS queries return the information that you
-- entered for the technical contact.
--
-- You must specify the same privacy setting for the administrative,
-- registrant, and technical contacts.
--
-- Default: @true@
transferDomain_privacyProtectTechContact :: Lens.Lens' TransferDomain (Prelude.Maybe Prelude.Bool)
transferDomain_privacyProtectTechContact = Lens.lens (\TransferDomain' {privacyProtectTechContact} -> privacyProtectTechContact) (\s@TransferDomain' {} a -> s {privacyProtectTechContact = a} :: TransferDomain)

-- | The name of the domain that you want to transfer to Route 53. The
-- top-level domain (TLD), such as .com, must be a TLD that Route 53
-- supports. For a list of supported TLDs, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53>
-- in the /Amazon Route 53 Developer Guide/.
--
-- The domain name can contain only the following characters:
--
-- -   Letters a through z. Domain names are not case sensitive.
--
-- -   Numbers 0 through 9.
--
-- -   Hyphen (-). You can\'t specify a hyphen at the beginning or end of a
--     label.
--
-- -   Period (.) to separate the labels in the name, such as the @.@ in
--     @example.com@.
transferDomain_domainName :: Lens.Lens' TransferDomain Prelude.Text
transferDomain_domainName = Lens.lens (\TransferDomain' {domainName} -> domainName) (\s@TransferDomain' {} a -> s {domainName = a} :: TransferDomain)

-- | The number of years that you want to register the domain for. Domains
-- are registered for a minimum of one year. The maximum period depends on
-- the top-level domain.
--
-- Default: 1
transferDomain_durationInYears :: Lens.Lens' TransferDomain Prelude.Natural
transferDomain_durationInYears = Lens.lens (\TransferDomain' {durationInYears} -> durationInYears) (\s@TransferDomain' {} a -> s {durationInYears = a} :: TransferDomain)

-- | Provides detailed contact information.
transferDomain_adminContact :: Lens.Lens' TransferDomain ContactDetail
transferDomain_adminContact = Lens.lens (\TransferDomain' {adminContact} -> adminContact) (\s@TransferDomain' {} a -> s {adminContact = a} :: TransferDomain) Prelude.. Data._Sensitive

-- | Provides detailed contact information.
transferDomain_registrantContact :: Lens.Lens' TransferDomain ContactDetail
transferDomain_registrantContact = Lens.lens (\TransferDomain' {registrantContact} -> registrantContact) (\s@TransferDomain' {} a -> s {registrantContact = a} :: TransferDomain) Prelude.. Data._Sensitive

-- | Provides detailed contact information.
transferDomain_techContact :: Lens.Lens' TransferDomain ContactDetail
transferDomain_techContact = Lens.lens (\TransferDomain' {techContact} -> techContact) (\s@TransferDomain' {} a -> s {techContact = a} :: TransferDomain) Prelude.. Data._Sensitive

instance Core.AWSRequest TransferDomain where
  type
    AWSResponse TransferDomain =
      TransferDomainResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          TransferDomainResponse'
            Prelude.<$> (x Data..?> "OperationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable TransferDomain where
  hashWithSalt _salt TransferDomain' {..} =
    _salt
      `Prelude.hashWithSalt` authCode
      `Prelude.hashWithSalt` autoRenew
      `Prelude.hashWithSalt` idnLangCode
      `Prelude.hashWithSalt` nameservers
      `Prelude.hashWithSalt` privacyProtectAdminContact
      `Prelude.hashWithSalt` privacyProtectRegistrantContact
      `Prelude.hashWithSalt` privacyProtectTechContact
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` durationInYears
      `Prelude.hashWithSalt` adminContact
      `Prelude.hashWithSalt` registrantContact
      `Prelude.hashWithSalt` techContact

instance Prelude.NFData TransferDomain where
  rnf TransferDomain' {..} =
    Prelude.rnf authCode
      `Prelude.seq` Prelude.rnf autoRenew
      `Prelude.seq` Prelude.rnf idnLangCode
      `Prelude.seq` Prelude.rnf nameservers
      `Prelude.seq` Prelude.rnf privacyProtectAdminContact
      `Prelude.seq` Prelude.rnf privacyProtectRegistrantContact
      `Prelude.seq` Prelude.rnf privacyProtectTechContact
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf durationInYears
      `Prelude.seq` Prelude.rnf adminContact
      `Prelude.seq` Prelude.rnf registrantContact
      `Prelude.seq` Prelude.rnf techContact

instance Data.ToHeaders TransferDomain where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Domains_v20140515.TransferDomain" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON TransferDomain where
  toJSON TransferDomain' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AuthCode" Data..=) Prelude.<$> authCode,
            ("AutoRenew" Data..=) Prelude.<$> autoRenew,
            ("IdnLangCode" Data..=) Prelude.<$> idnLangCode,
            ("Nameservers" Data..=) Prelude.<$> nameservers,
            ("PrivacyProtectAdminContact" Data..=)
              Prelude.<$> privacyProtectAdminContact,
            ("PrivacyProtectRegistrantContact" Data..=)
              Prelude.<$> privacyProtectRegistrantContact,
            ("PrivacyProtectTechContact" Data..=)
              Prelude.<$> privacyProtectTechContact,
            Prelude.Just ("DomainName" Data..= domainName),
            Prelude.Just
              ("DurationInYears" Data..= durationInYears),
            Prelude.Just ("AdminContact" Data..= adminContact),
            Prelude.Just
              ("RegistrantContact" Data..= registrantContact),
            Prelude.Just ("TechContact" Data..= techContact)
          ]
      )

instance Data.ToPath TransferDomain where
  toPath = Prelude.const "/"

instance Data.ToQuery TransferDomain where
  toQuery = Prelude.const Prelude.mempty

-- | The TransferDomain response includes the following element.
--
-- /See:/ 'newTransferDomainResponse' smart constructor.
data TransferDomainResponse = TransferDomainResponse'
  { -- | Identifier for tracking the progress of the request. To query the
    -- operation status, use
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail>.
    operationId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransferDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operationId', 'transferDomainResponse_operationId' - Identifier for tracking the progress of the request. To query the
-- operation status, use
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail>.
--
-- 'httpStatus', 'transferDomainResponse_httpStatus' - The response's http status code.
newTransferDomainResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  TransferDomainResponse
newTransferDomainResponse pHttpStatus_ =
  TransferDomainResponse'
    { operationId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Identifier for tracking the progress of the request. To query the
-- operation status, use
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail>.
transferDomainResponse_operationId :: Lens.Lens' TransferDomainResponse (Prelude.Maybe Prelude.Text)
transferDomainResponse_operationId = Lens.lens (\TransferDomainResponse' {operationId} -> operationId) (\s@TransferDomainResponse' {} a -> s {operationId = a} :: TransferDomainResponse)

-- | The response's http status code.
transferDomainResponse_httpStatus :: Lens.Lens' TransferDomainResponse Prelude.Int
transferDomainResponse_httpStatus = Lens.lens (\TransferDomainResponse' {httpStatus} -> httpStatus) (\s@TransferDomainResponse' {} a -> s {httpStatus = a} :: TransferDomainResponse)

instance Prelude.NFData TransferDomainResponse where
  rnf TransferDomainResponse' {..} =
    Prelude.rnf operationId
      `Prelude.seq` Prelude.rnf httpStatus
