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
-- Module      : Network.AWS.Route53Domains.TransferDomain
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
-- -   For information about how to transfer a domain from one AWS account
--     to another, see
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
module Network.AWS.Route53Domains.TransferDomain
  ( -- * Creating a Request
    TransferDomain (..),
    newTransferDomain,

    -- * Request Lenses
    transferDomain_autoRenew,
    transferDomain_nameservers,
    transferDomain_authCode,
    transferDomain_idnLangCode,
    transferDomain_privacyProtectTechContact,
    transferDomain_privacyProtectRegistrantContact,
    transferDomain_privacyProtectAdminContact,
    transferDomain_domainName,
    transferDomain_durationInYears,
    transferDomain_adminContact,
    transferDomain_registrantContact,
    transferDomain_techContact,

    -- * Destructuring the Response
    TransferDomainResponse (..),
    newTransferDomainResponse,

    -- * Response Lenses
    transferDomainResponse_httpStatus,
    transferDomainResponse_operationId,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53Domains.Types

-- | The TransferDomain request includes the following elements.
--
-- /See:/ 'newTransferDomain' smart constructor.
data TransferDomain = TransferDomain'
  { -- | Indicates whether the domain will be automatically renewed (true) or not
    -- (false). Autorenewal only takes effect after the account is charged.
    --
    -- Default: true
    autoRenew :: Core.Maybe Core.Bool,
    -- | Contains details for the host and glue IP addresses.
    nameservers :: Core.Maybe [Nameserver],
    -- | The authorization code for the domain. You get this value from the
    -- current registrar.
    authCode :: Core.Maybe (Core.Sensitive Core.Text),
    -- | Reserved for future use.
    idnLangCode :: Core.Maybe Core.Text,
    -- | Whether you want to conceal contact information from WHOIS queries. If
    -- you specify @true@, WHOIS (\"who is\") queries return contact
    -- information either for Amazon Registrar (for .com, .net, and .org
    -- domains) or for our registrar associate, Gandi (for all other TLDs). If
    -- you specify @false@, WHOIS queries return the information that you
    -- entered for the technical contact.
    --
    -- Default: @true@
    privacyProtectTechContact :: Core.Maybe Core.Bool,
    -- | Whether you want to conceal contact information from WHOIS queries. If
    -- you specify @true@, WHOIS (\"who is\") queries return contact
    -- information either for Amazon Registrar (for .com, .net, and .org
    -- domains) or for our registrar associate, Gandi (for all other TLDs). If
    -- you specify @false@, WHOIS queries return the information that you
    -- entered for the registrant contact (domain owner).
    --
    -- Default: @true@
    privacyProtectRegistrantContact :: Core.Maybe Core.Bool,
    -- | Whether you want to conceal contact information from WHOIS queries. If
    -- you specify @true@, WHOIS (\"who is\") queries return contact
    -- information either for Amazon Registrar (for .com, .net, and .org
    -- domains) or for our registrar associate, Gandi (for all other TLDs). If
    -- you specify @false@, WHOIS queries return the information that you
    -- entered for the admin contact.
    --
    -- Default: @true@
    privacyProtectAdminContact :: Core.Maybe Core.Bool,
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
    domainName :: Core.Text,
    -- | The number of years that you want to register the domain for. Domains
    -- are registered for a minimum of one year. The maximum period depends on
    -- the top-level domain.
    --
    -- Default: 1
    durationInYears :: Core.Natural,
    -- | Provides detailed contact information.
    adminContact :: Core.Sensitive ContactDetail,
    -- | Provides detailed contact information.
    registrantContact :: Core.Sensitive ContactDetail,
    -- | Provides detailed contact information.
    techContact :: Core.Sensitive ContactDetail
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'TransferDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoRenew', 'transferDomain_autoRenew' - Indicates whether the domain will be automatically renewed (true) or not
-- (false). Autorenewal only takes effect after the account is charged.
--
-- Default: true
--
-- 'nameservers', 'transferDomain_nameservers' - Contains details for the host and glue IP addresses.
--
-- 'authCode', 'transferDomain_authCode' - The authorization code for the domain. You get this value from the
-- current registrar.
--
-- 'idnLangCode', 'transferDomain_idnLangCode' - Reserved for future use.
--
-- 'privacyProtectTechContact', 'transferDomain_privacyProtectTechContact' - Whether you want to conceal contact information from WHOIS queries. If
-- you specify @true@, WHOIS (\"who is\") queries return contact
-- information either for Amazon Registrar (for .com, .net, and .org
-- domains) or for our registrar associate, Gandi (for all other TLDs). If
-- you specify @false@, WHOIS queries return the information that you
-- entered for the technical contact.
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
-- Default: @true@
--
-- 'privacyProtectAdminContact', 'transferDomain_privacyProtectAdminContact' - Whether you want to conceal contact information from WHOIS queries. If
-- you specify @true@, WHOIS (\"who is\") queries return contact
-- information either for Amazon Registrar (for .com, .net, and .org
-- domains) or for our registrar associate, Gandi (for all other TLDs). If
-- you specify @false@, WHOIS queries return the information that you
-- entered for the admin contact.
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
  Core.Text ->
  -- | 'durationInYears'
  Core.Natural ->
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
      { autoRenew = Core.Nothing,
        nameservers = Core.Nothing,
        authCode = Core.Nothing,
        idnLangCode = Core.Nothing,
        privacyProtectTechContact = Core.Nothing,
        privacyProtectRegistrantContact = Core.Nothing,
        privacyProtectAdminContact = Core.Nothing,
        domainName = pDomainName_,
        durationInYears = pDurationInYears_,
        adminContact = Core._Sensitive Lens.# pAdminContact_,
        registrantContact =
          Core._Sensitive Lens.# pRegistrantContact_,
        techContact = Core._Sensitive Lens.# pTechContact_
      }

-- | Indicates whether the domain will be automatically renewed (true) or not
-- (false). Autorenewal only takes effect after the account is charged.
--
-- Default: true
transferDomain_autoRenew :: Lens.Lens' TransferDomain (Core.Maybe Core.Bool)
transferDomain_autoRenew = Lens.lens (\TransferDomain' {autoRenew} -> autoRenew) (\s@TransferDomain' {} a -> s {autoRenew = a} :: TransferDomain)

-- | Contains details for the host and glue IP addresses.
transferDomain_nameservers :: Lens.Lens' TransferDomain (Core.Maybe [Nameserver])
transferDomain_nameservers = Lens.lens (\TransferDomain' {nameservers} -> nameservers) (\s@TransferDomain' {} a -> s {nameservers = a} :: TransferDomain) Core.. Lens.mapping Lens._Coerce

-- | The authorization code for the domain. You get this value from the
-- current registrar.
transferDomain_authCode :: Lens.Lens' TransferDomain (Core.Maybe Core.Text)
transferDomain_authCode = Lens.lens (\TransferDomain' {authCode} -> authCode) (\s@TransferDomain' {} a -> s {authCode = a} :: TransferDomain) Core.. Lens.mapping Core._Sensitive

-- | Reserved for future use.
transferDomain_idnLangCode :: Lens.Lens' TransferDomain (Core.Maybe Core.Text)
transferDomain_idnLangCode = Lens.lens (\TransferDomain' {idnLangCode} -> idnLangCode) (\s@TransferDomain' {} a -> s {idnLangCode = a} :: TransferDomain)

-- | Whether you want to conceal contact information from WHOIS queries. If
-- you specify @true@, WHOIS (\"who is\") queries return contact
-- information either for Amazon Registrar (for .com, .net, and .org
-- domains) or for our registrar associate, Gandi (for all other TLDs). If
-- you specify @false@, WHOIS queries return the information that you
-- entered for the technical contact.
--
-- Default: @true@
transferDomain_privacyProtectTechContact :: Lens.Lens' TransferDomain (Core.Maybe Core.Bool)
transferDomain_privacyProtectTechContact = Lens.lens (\TransferDomain' {privacyProtectTechContact} -> privacyProtectTechContact) (\s@TransferDomain' {} a -> s {privacyProtectTechContact = a} :: TransferDomain)

-- | Whether you want to conceal contact information from WHOIS queries. If
-- you specify @true@, WHOIS (\"who is\") queries return contact
-- information either for Amazon Registrar (for .com, .net, and .org
-- domains) or for our registrar associate, Gandi (for all other TLDs). If
-- you specify @false@, WHOIS queries return the information that you
-- entered for the registrant contact (domain owner).
--
-- Default: @true@
transferDomain_privacyProtectRegistrantContact :: Lens.Lens' TransferDomain (Core.Maybe Core.Bool)
transferDomain_privacyProtectRegistrantContact = Lens.lens (\TransferDomain' {privacyProtectRegistrantContact} -> privacyProtectRegistrantContact) (\s@TransferDomain' {} a -> s {privacyProtectRegistrantContact = a} :: TransferDomain)

-- | Whether you want to conceal contact information from WHOIS queries. If
-- you specify @true@, WHOIS (\"who is\") queries return contact
-- information either for Amazon Registrar (for .com, .net, and .org
-- domains) or for our registrar associate, Gandi (for all other TLDs). If
-- you specify @false@, WHOIS queries return the information that you
-- entered for the admin contact.
--
-- Default: @true@
transferDomain_privacyProtectAdminContact :: Lens.Lens' TransferDomain (Core.Maybe Core.Bool)
transferDomain_privacyProtectAdminContact = Lens.lens (\TransferDomain' {privacyProtectAdminContact} -> privacyProtectAdminContact) (\s@TransferDomain' {} a -> s {privacyProtectAdminContact = a} :: TransferDomain)

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
transferDomain_domainName :: Lens.Lens' TransferDomain Core.Text
transferDomain_domainName = Lens.lens (\TransferDomain' {domainName} -> domainName) (\s@TransferDomain' {} a -> s {domainName = a} :: TransferDomain)

-- | The number of years that you want to register the domain for. Domains
-- are registered for a minimum of one year. The maximum period depends on
-- the top-level domain.
--
-- Default: 1
transferDomain_durationInYears :: Lens.Lens' TransferDomain Core.Natural
transferDomain_durationInYears = Lens.lens (\TransferDomain' {durationInYears} -> durationInYears) (\s@TransferDomain' {} a -> s {durationInYears = a} :: TransferDomain)

-- | Provides detailed contact information.
transferDomain_adminContact :: Lens.Lens' TransferDomain ContactDetail
transferDomain_adminContact = Lens.lens (\TransferDomain' {adminContact} -> adminContact) (\s@TransferDomain' {} a -> s {adminContact = a} :: TransferDomain) Core.. Core._Sensitive

-- | Provides detailed contact information.
transferDomain_registrantContact :: Lens.Lens' TransferDomain ContactDetail
transferDomain_registrantContact = Lens.lens (\TransferDomain' {registrantContact} -> registrantContact) (\s@TransferDomain' {} a -> s {registrantContact = a} :: TransferDomain) Core.. Core._Sensitive

-- | Provides detailed contact information.
transferDomain_techContact :: Lens.Lens' TransferDomain ContactDetail
transferDomain_techContact = Lens.lens (\TransferDomain' {techContact} -> techContact) (\s@TransferDomain' {} a -> s {techContact = a} :: TransferDomain) Core.. Core._Sensitive

instance Core.AWSRequest TransferDomain where
  type
    AWSResponse TransferDomain =
      TransferDomainResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          TransferDomainResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "OperationId")
      )

instance Core.Hashable TransferDomain

instance Core.NFData TransferDomain

instance Core.ToHeaders TransferDomain where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Route53Domains_v20140515.TransferDomain" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON TransferDomain where
  toJSON TransferDomain' {..} =
    Core.object
      ( Core.catMaybes
          [ ("AutoRenew" Core..=) Core.<$> autoRenew,
            ("Nameservers" Core..=) Core.<$> nameservers,
            ("AuthCode" Core..=) Core.<$> authCode,
            ("IdnLangCode" Core..=) Core.<$> idnLangCode,
            ("PrivacyProtectTechContact" Core..=)
              Core.<$> privacyProtectTechContact,
            ("PrivacyProtectRegistrantContact" Core..=)
              Core.<$> privacyProtectRegistrantContact,
            ("PrivacyProtectAdminContact" Core..=)
              Core.<$> privacyProtectAdminContact,
            Core.Just ("DomainName" Core..= domainName),
            Core.Just
              ("DurationInYears" Core..= durationInYears),
            Core.Just ("AdminContact" Core..= adminContact),
            Core.Just
              ("RegistrantContact" Core..= registrantContact),
            Core.Just ("TechContact" Core..= techContact)
          ]
      )

instance Core.ToPath TransferDomain where
  toPath = Core.const "/"

instance Core.ToQuery TransferDomain where
  toQuery = Core.const Core.mempty

-- | The TransferDomain response includes the following element.
--
-- /See:/ 'newTransferDomainResponse' smart constructor.
data TransferDomainResponse = TransferDomainResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | Identifier for tracking the progress of the request. To query the
    -- operation status, use
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail>.
    operationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TransferDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'transferDomainResponse_httpStatus' - The response's http status code.
--
-- 'operationId', 'transferDomainResponse_operationId' - Identifier for tracking the progress of the request. To query the
-- operation status, use
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail>.
newTransferDomainResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'operationId'
  Core.Text ->
  TransferDomainResponse
newTransferDomainResponse pHttpStatus_ pOperationId_ =
  TransferDomainResponse'
    { httpStatus = pHttpStatus_,
      operationId = pOperationId_
    }

-- | The response's http status code.
transferDomainResponse_httpStatus :: Lens.Lens' TransferDomainResponse Core.Int
transferDomainResponse_httpStatus = Lens.lens (\TransferDomainResponse' {httpStatus} -> httpStatus) (\s@TransferDomainResponse' {} a -> s {httpStatus = a} :: TransferDomainResponse)

-- | Identifier for tracking the progress of the request. To query the
-- operation status, use
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail>.
transferDomainResponse_operationId :: Lens.Lens' TransferDomainResponse Core.Text
transferDomainResponse_operationId = Lens.lens (\TransferDomainResponse' {operationId} -> operationId) (\s@TransferDomainResponse' {} a -> s {operationId = a} :: TransferDomainResponse)

instance Core.NFData TransferDomainResponse
