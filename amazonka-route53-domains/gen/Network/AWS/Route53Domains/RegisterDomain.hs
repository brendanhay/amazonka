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
-- Module      : Network.AWS.Route53Domains.RegisterDomain
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation registers a domain. Domains are registered either by
-- Amazon Registrar (for .com, .net, and .org domains) or by our registrar
-- associate, Gandi (for all other domains). For some top-level domains
-- (TLDs), this operation requires extra parameters.
--
-- When you register a domain, Amazon Route 53 does the following:
--
-- -   Creates a Route 53 hosted zone that has the same name as the domain.
--     Route 53 assigns four name servers to your hosted zone and
--     automatically updates your domain registration with the names of
--     these name servers.
--
-- -   Enables autorenew, so your domain registration will renew
--     automatically each year. We\'ll notify you in advance of the renewal
--     date so you can choose whether to renew the registration.
--
-- -   Optionally enables privacy protection, so WHOIS queries return
--     contact information either for Amazon Registrar (for .com, .net, and
--     .org domains) or for our registrar associate, Gandi (for all other
--     TLDs). If you don\'t enable privacy protection, WHOIS queries return
--     the information that you entered for the registrant, admin, and tech
--     contacts.
--
-- -   If registration is successful, returns an operation ID that you can
--     use to track the progress and completion of the action. If the
--     request is not completed successfully, the domain registrant is
--     notified by email.
--
-- -   Charges your AWS account an amount based on the top-level domain.
--     For more information, see
--     <http://aws.amazon.com/route53/pricing/ Amazon Route 53 Pricing>.
module Network.AWS.Route53Domains.RegisterDomain
  ( -- * Creating a Request
    RegisterDomain (..),
    newRegisterDomain,

    -- * Request Lenses
    registerDomain_autoRenew,
    registerDomain_idnLangCode,
    registerDomain_privacyProtectTechContact,
    registerDomain_privacyProtectRegistrantContact,
    registerDomain_privacyProtectAdminContact,
    registerDomain_domainName,
    registerDomain_durationInYears,
    registerDomain_adminContact,
    registerDomain_registrantContact,
    registerDomain_techContact,

    -- * Destructuring the Response
    RegisterDomainResponse (..),
    newRegisterDomainResponse,

    -- * Response Lenses
    registerDomainResponse_httpStatus,
    registerDomainResponse_operationId,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53Domains.Types

-- | The RegisterDomain request includes the following elements.
--
-- /See:/ 'newRegisterDomain' smart constructor.
data RegisterDomain = RegisterDomain'
  { -- | Indicates whether the domain will be automatically renewed (@true@) or
    -- not (@false@). Autorenewal only takes effect after the account is
    -- charged.
    --
    -- Default: @true@
    autoRenew :: Prelude.Maybe Prelude.Bool,
    -- | Reserved for future use.
    idnLangCode :: Prelude.Maybe Prelude.Text,
    -- | Whether you want to conceal contact information from WHOIS queries. If
    -- you specify @true@, WHOIS (\"who is\") queries return contact
    -- information either for Amazon Registrar (for .com, .net, and .org
    -- domains) or for our registrar associate, Gandi (for all other TLDs). If
    -- you specify @false@, WHOIS queries return the information that you
    -- entered for the technical contact.
    --
    -- Default: @true@
    privacyProtectTechContact :: Prelude.Maybe Prelude.Bool,
    -- | Whether you want to conceal contact information from WHOIS queries. If
    -- you specify @true@, WHOIS (\"who is\") queries return contact
    -- information either for Amazon Registrar (for .com, .net, and .org
    -- domains) or for our registrar associate, Gandi (for all other TLDs). If
    -- you specify @false@, WHOIS queries return the information that you
    -- entered for the registrant contact (the domain owner).
    --
    -- Default: @true@
    privacyProtectRegistrantContact :: Prelude.Maybe Prelude.Bool,
    -- | Whether you want to conceal contact information from WHOIS queries. If
    -- you specify @true@, WHOIS (\"who is\") queries return contact
    -- information either for Amazon Registrar (for .com, .net, and .org
    -- domains) or for our registrar associate, Gandi (for all other TLDs). If
    -- you specify @false@, WHOIS queries return the information that you
    -- entered for the admin contact.
    --
    -- Default: @true@
    privacyProtectAdminContact :: Prelude.Maybe Prelude.Bool,
    -- | The domain name that you want to register. The top-level domain (TLD),
    -- such as .com, must be a TLD that Route 53 supports. For a list of
    -- supported TLDs, see
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
    -- Internationalized domain names are not supported for some top-level
    -- domains. To determine whether the TLD that you want to use supports
    -- internationalized domain names, see
    -- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53>.
    -- For more information, see
    -- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DomainNameFormat.html#domain-name-format-idns Formatting Internationalized Domain Names>.
    domainName :: Prelude.Text,
    -- | The number of years that you want to register the domain for. Domains
    -- are registered for a minimum of one year. The maximum period depends on
    -- the top-level domain. For the range of valid values for your domain, see
    -- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53>
    -- in the /Amazon Route 53 Developer Guide/.
    --
    -- Default: 1
    durationInYears :: Prelude.Natural,
    -- | Provides detailed contact information. For information about the values
    -- that you specify for each element, see
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_ContactDetail.html ContactDetail>.
    adminContact :: Core.Sensitive ContactDetail,
    -- | Provides detailed contact information. For information about the values
    -- that you specify for each element, see
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_ContactDetail.html ContactDetail>.
    registrantContact :: Core.Sensitive ContactDetail,
    -- | Provides detailed contact information. For information about the values
    -- that you specify for each element, see
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_ContactDetail.html ContactDetail>.
    techContact :: Core.Sensitive ContactDetail
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoRenew', 'registerDomain_autoRenew' - Indicates whether the domain will be automatically renewed (@true@) or
-- not (@false@). Autorenewal only takes effect after the account is
-- charged.
--
-- Default: @true@
--
-- 'idnLangCode', 'registerDomain_idnLangCode' - Reserved for future use.
--
-- 'privacyProtectTechContact', 'registerDomain_privacyProtectTechContact' - Whether you want to conceal contact information from WHOIS queries. If
-- you specify @true@, WHOIS (\"who is\") queries return contact
-- information either for Amazon Registrar (for .com, .net, and .org
-- domains) or for our registrar associate, Gandi (for all other TLDs). If
-- you specify @false@, WHOIS queries return the information that you
-- entered for the technical contact.
--
-- Default: @true@
--
-- 'privacyProtectRegistrantContact', 'registerDomain_privacyProtectRegistrantContact' - Whether you want to conceal contact information from WHOIS queries. If
-- you specify @true@, WHOIS (\"who is\") queries return contact
-- information either for Amazon Registrar (for .com, .net, and .org
-- domains) or for our registrar associate, Gandi (for all other TLDs). If
-- you specify @false@, WHOIS queries return the information that you
-- entered for the registrant contact (the domain owner).
--
-- Default: @true@
--
-- 'privacyProtectAdminContact', 'registerDomain_privacyProtectAdminContact' - Whether you want to conceal contact information from WHOIS queries. If
-- you specify @true@, WHOIS (\"who is\") queries return contact
-- information either for Amazon Registrar (for .com, .net, and .org
-- domains) or for our registrar associate, Gandi (for all other TLDs). If
-- you specify @false@, WHOIS queries return the information that you
-- entered for the admin contact.
--
-- Default: @true@
--
-- 'domainName', 'registerDomain_domainName' - The domain name that you want to register. The top-level domain (TLD),
-- such as .com, must be a TLD that Route 53 supports. For a list of
-- supported TLDs, see
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
-- Internationalized domain names are not supported for some top-level
-- domains. To determine whether the TLD that you want to use supports
-- internationalized domain names, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53>.
-- For more information, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DomainNameFormat.html#domain-name-format-idns Formatting Internationalized Domain Names>.
--
-- 'durationInYears', 'registerDomain_durationInYears' - The number of years that you want to register the domain for. Domains
-- are registered for a minimum of one year. The maximum period depends on
-- the top-level domain. For the range of valid values for your domain, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53>
-- in the /Amazon Route 53 Developer Guide/.
--
-- Default: 1
--
-- 'adminContact', 'registerDomain_adminContact' - Provides detailed contact information. For information about the values
-- that you specify for each element, see
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_ContactDetail.html ContactDetail>.
--
-- 'registrantContact', 'registerDomain_registrantContact' - Provides detailed contact information. For information about the values
-- that you specify for each element, see
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_ContactDetail.html ContactDetail>.
--
-- 'techContact', 'registerDomain_techContact' - Provides detailed contact information. For information about the values
-- that you specify for each element, see
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_ContactDetail.html ContactDetail>.
newRegisterDomain ::
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
  RegisterDomain
newRegisterDomain
  pDomainName_
  pDurationInYears_
  pAdminContact_
  pRegistrantContact_
  pTechContact_ =
    RegisterDomain'
      { autoRenew = Prelude.Nothing,
        idnLangCode = Prelude.Nothing,
        privacyProtectTechContact = Prelude.Nothing,
        privacyProtectRegistrantContact = Prelude.Nothing,
        privacyProtectAdminContact = Prelude.Nothing,
        domainName = pDomainName_,
        durationInYears = pDurationInYears_,
        adminContact = Core._Sensitive Lens.# pAdminContact_,
        registrantContact =
          Core._Sensitive Lens.# pRegistrantContact_,
        techContact = Core._Sensitive Lens.# pTechContact_
      }

-- | Indicates whether the domain will be automatically renewed (@true@) or
-- not (@false@). Autorenewal only takes effect after the account is
-- charged.
--
-- Default: @true@
registerDomain_autoRenew :: Lens.Lens' RegisterDomain (Prelude.Maybe Prelude.Bool)
registerDomain_autoRenew = Lens.lens (\RegisterDomain' {autoRenew} -> autoRenew) (\s@RegisterDomain' {} a -> s {autoRenew = a} :: RegisterDomain)

-- | Reserved for future use.
registerDomain_idnLangCode :: Lens.Lens' RegisterDomain (Prelude.Maybe Prelude.Text)
registerDomain_idnLangCode = Lens.lens (\RegisterDomain' {idnLangCode} -> idnLangCode) (\s@RegisterDomain' {} a -> s {idnLangCode = a} :: RegisterDomain)

-- | Whether you want to conceal contact information from WHOIS queries. If
-- you specify @true@, WHOIS (\"who is\") queries return contact
-- information either for Amazon Registrar (for .com, .net, and .org
-- domains) or for our registrar associate, Gandi (for all other TLDs). If
-- you specify @false@, WHOIS queries return the information that you
-- entered for the technical contact.
--
-- Default: @true@
registerDomain_privacyProtectTechContact :: Lens.Lens' RegisterDomain (Prelude.Maybe Prelude.Bool)
registerDomain_privacyProtectTechContact = Lens.lens (\RegisterDomain' {privacyProtectTechContact} -> privacyProtectTechContact) (\s@RegisterDomain' {} a -> s {privacyProtectTechContact = a} :: RegisterDomain)

-- | Whether you want to conceal contact information from WHOIS queries. If
-- you specify @true@, WHOIS (\"who is\") queries return contact
-- information either for Amazon Registrar (for .com, .net, and .org
-- domains) or for our registrar associate, Gandi (for all other TLDs). If
-- you specify @false@, WHOIS queries return the information that you
-- entered for the registrant contact (the domain owner).
--
-- Default: @true@
registerDomain_privacyProtectRegistrantContact :: Lens.Lens' RegisterDomain (Prelude.Maybe Prelude.Bool)
registerDomain_privacyProtectRegistrantContact = Lens.lens (\RegisterDomain' {privacyProtectRegistrantContact} -> privacyProtectRegistrantContact) (\s@RegisterDomain' {} a -> s {privacyProtectRegistrantContact = a} :: RegisterDomain)

-- | Whether you want to conceal contact information from WHOIS queries. If
-- you specify @true@, WHOIS (\"who is\") queries return contact
-- information either for Amazon Registrar (for .com, .net, and .org
-- domains) or for our registrar associate, Gandi (for all other TLDs). If
-- you specify @false@, WHOIS queries return the information that you
-- entered for the admin contact.
--
-- Default: @true@
registerDomain_privacyProtectAdminContact :: Lens.Lens' RegisterDomain (Prelude.Maybe Prelude.Bool)
registerDomain_privacyProtectAdminContact = Lens.lens (\RegisterDomain' {privacyProtectAdminContact} -> privacyProtectAdminContact) (\s@RegisterDomain' {} a -> s {privacyProtectAdminContact = a} :: RegisterDomain)

-- | The domain name that you want to register. The top-level domain (TLD),
-- such as .com, must be a TLD that Route 53 supports. For a list of
-- supported TLDs, see
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
-- Internationalized domain names are not supported for some top-level
-- domains. To determine whether the TLD that you want to use supports
-- internationalized domain names, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53>.
-- For more information, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DomainNameFormat.html#domain-name-format-idns Formatting Internationalized Domain Names>.
registerDomain_domainName :: Lens.Lens' RegisterDomain Prelude.Text
registerDomain_domainName = Lens.lens (\RegisterDomain' {domainName} -> domainName) (\s@RegisterDomain' {} a -> s {domainName = a} :: RegisterDomain)

-- | The number of years that you want to register the domain for. Domains
-- are registered for a minimum of one year. The maximum period depends on
-- the top-level domain. For the range of valid values for your domain, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53>
-- in the /Amazon Route 53 Developer Guide/.
--
-- Default: 1
registerDomain_durationInYears :: Lens.Lens' RegisterDomain Prelude.Natural
registerDomain_durationInYears = Lens.lens (\RegisterDomain' {durationInYears} -> durationInYears) (\s@RegisterDomain' {} a -> s {durationInYears = a} :: RegisterDomain)

-- | Provides detailed contact information. For information about the values
-- that you specify for each element, see
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_ContactDetail.html ContactDetail>.
registerDomain_adminContact :: Lens.Lens' RegisterDomain ContactDetail
registerDomain_adminContact = Lens.lens (\RegisterDomain' {adminContact} -> adminContact) (\s@RegisterDomain' {} a -> s {adminContact = a} :: RegisterDomain) Prelude.. Core._Sensitive

-- | Provides detailed contact information. For information about the values
-- that you specify for each element, see
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_ContactDetail.html ContactDetail>.
registerDomain_registrantContact :: Lens.Lens' RegisterDomain ContactDetail
registerDomain_registrantContact = Lens.lens (\RegisterDomain' {registrantContact} -> registrantContact) (\s@RegisterDomain' {} a -> s {registrantContact = a} :: RegisterDomain) Prelude.. Core._Sensitive

-- | Provides detailed contact information. For information about the values
-- that you specify for each element, see
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_ContactDetail.html ContactDetail>.
registerDomain_techContact :: Lens.Lens' RegisterDomain ContactDetail
registerDomain_techContact = Lens.lens (\RegisterDomain' {techContact} -> techContact) (\s@RegisterDomain' {} a -> s {techContact = a} :: RegisterDomain) Prelude.. Core._Sensitive

instance Core.AWSRequest RegisterDomain where
  type
    AWSResponse RegisterDomain =
      RegisterDomainResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterDomainResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "OperationId")
      )

instance Prelude.Hashable RegisterDomain

instance Prelude.NFData RegisterDomain

instance Core.ToHeaders RegisterDomain where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Route53Domains_v20140515.RegisterDomain" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON RegisterDomain where
  toJSON RegisterDomain' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AutoRenew" Core..=) Prelude.<$> autoRenew,
            ("IdnLangCode" Core..=) Prelude.<$> idnLangCode,
            ("PrivacyProtectTechContact" Core..=)
              Prelude.<$> privacyProtectTechContact,
            ("PrivacyProtectRegistrantContact" Core..=)
              Prelude.<$> privacyProtectRegistrantContact,
            ("PrivacyProtectAdminContact" Core..=)
              Prelude.<$> privacyProtectAdminContact,
            Prelude.Just ("DomainName" Core..= domainName),
            Prelude.Just
              ("DurationInYears" Core..= durationInYears),
            Prelude.Just ("AdminContact" Core..= adminContact),
            Prelude.Just
              ("RegistrantContact" Core..= registrantContact),
            Prelude.Just ("TechContact" Core..= techContact)
          ]
      )

instance Core.ToPath RegisterDomain where
  toPath = Prelude.const "/"

instance Core.ToQuery RegisterDomain where
  toQuery = Prelude.const Prelude.mempty

-- | The RegisterDomain response includes the following element.
--
-- /See:/ 'newRegisterDomainResponse' smart constructor.
data RegisterDomainResponse = RegisterDomainResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Identifier for tracking the progress of the request. To query the
    -- operation status, use
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail>.
    operationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'registerDomainResponse_httpStatus' - The response's http status code.
--
-- 'operationId', 'registerDomainResponse_operationId' - Identifier for tracking the progress of the request. To query the
-- operation status, use
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail>.
newRegisterDomainResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'operationId'
  Prelude.Text ->
  RegisterDomainResponse
newRegisterDomainResponse pHttpStatus_ pOperationId_ =
  RegisterDomainResponse'
    { httpStatus = pHttpStatus_,
      operationId = pOperationId_
    }

-- | The response's http status code.
registerDomainResponse_httpStatus :: Lens.Lens' RegisterDomainResponse Prelude.Int
registerDomainResponse_httpStatus = Lens.lens (\RegisterDomainResponse' {httpStatus} -> httpStatus) (\s@RegisterDomainResponse' {} a -> s {httpStatus = a} :: RegisterDomainResponse)

-- | Identifier for tracking the progress of the request. To query the
-- operation status, use
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail>.
registerDomainResponse_operationId :: Lens.Lens' RegisterDomainResponse Prelude.Text
registerDomainResponse_operationId = Lens.lens (\RegisterDomainResponse' {operationId} -> operationId) (\s@RegisterDomainResponse' {} a -> s {operationId = a} :: RegisterDomainResponse)

instance Prelude.NFData RegisterDomainResponse
