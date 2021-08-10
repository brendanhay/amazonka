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
-- Module      : Network.AWS.Route53Domains.GetDomainDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns detailed information about a specified domain
-- that is associated with the current AWS account. Contact information for
-- the domain is also returned as part of the output.
module Network.AWS.Route53Domains.GetDomainDetail
  ( -- * Creating a Request
    GetDomainDetail (..),
    newGetDomainDetail,

    -- * Request Lenses
    getDomainDetail_domainName,

    -- * Destructuring the Response
    GetDomainDetailResponse (..),
    newGetDomainDetailResponse,

    -- * Response Lenses
    getDomainDetailResponse_dnsSec,
    getDomainDetailResponse_abuseContactPhone,
    getDomainDetailResponse_autoRenew,
    getDomainDetailResponse_abuseContactEmail,
    getDomainDetailResponse_adminPrivacy,
    getDomainDetailResponse_statusList,
    getDomainDetailResponse_reseller,
    getDomainDetailResponse_registrarName,
    getDomainDetailResponse_registryDomainId,
    getDomainDetailResponse_creationDate,
    getDomainDetailResponse_expirationDate,
    getDomainDetailResponse_whoIsServer,
    getDomainDetailResponse_registrarUrl,
    getDomainDetailResponse_techPrivacy,
    getDomainDetailResponse_registrantPrivacy,
    getDomainDetailResponse_updatedDate,
    getDomainDetailResponse_httpStatus,
    getDomainDetailResponse_domainName,
    getDomainDetailResponse_nameservers,
    getDomainDetailResponse_adminContact,
    getDomainDetailResponse_registrantContact,
    getDomainDetailResponse_techContact,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53Domains.Types

-- | The GetDomainDetail request includes the following element.
--
-- /See:/ 'newGetDomainDetail' smart constructor.
data GetDomainDetail = GetDomainDetail'
  { -- | The name of the domain that you want to get detailed information about.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDomainDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'getDomainDetail_domainName' - The name of the domain that you want to get detailed information about.
newGetDomainDetail ::
  -- | 'domainName'
  Prelude.Text ->
  GetDomainDetail
newGetDomainDetail pDomainName_ =
  GetDomainDetail' {domainName = pDomainName_}

-- | The name of the domain that you want to get detailed information about.
getDomainDetail_domainName :: Lens.Lens' GetDomainDetail Prelude.Text
getDomainDetail_domainName = Lens.lens (\GetDomainDetail' {domainName} -> domainName) (\s@GetDomainDetail' {} a -> s {domainName = a} :: GetDomainDetail)

instance Core.AWSRequest GetDomainDetail where
  type
    AWSResponse GetDomainDetail =
      GetDomainDetailResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDomainDetailResponse'
            Prelude.<$> (x Core..?> "DnsSec")
            Prelude.<*> (x Core..?> "AbuseContactPhone")
            Prelude.<*> (x Core..?> "AutoRenew")
            Prelude.<*> (x Core..?> "AbuseContactEmail")
            Prelude.<*> (x Core..?> "AdminPrivacy")
            Prelude.<*> (x Core..?> "StatusList" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "Reseller")
            Prelude.<*> (x Core..?> "RegistrarName")
            Prelude.<*> (x Core..?> "RegistryDomainId")
            Prelude.<*> (x Core..?> "CreationDate")
            Prelude.<*> (x Core..?> "ExpirationDate")
            Prelude.<*> (x Core..?> "WhoIsServer")
            Prelude.<*> (x Core..?> "RegistrarUrl")
            Prelude.<*> (x Core..?> "TechPrivacy")
            Prelude.<*> (x Core..?> "RegistrantPrivacy")
            Prelude.<*> (x Core..?> "UpdatedDate")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "DomainName")
            Prelude.<*> (x Core..?> "Nameservers" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..:> "AdminContact")
            Prelude.<*> (x Core..:> "RegistrantContact")
            Prelude.<*> (x Core..:> "TechContact")
      )

instance Prelude.Hashable GetDomainDetail

instance Prelude.NFData GetDomainDetail

instance Core.ToHeaders GetDomainDetail where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Route53Domains_v20140515.GetDomainDetail" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetDomainDetail where
  toJSON GetDomainDetail' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("DomainName" Core..= domainName)]
      )

instance Core.ToPath GetDomainDetail where
  toPath = Prelude.const "/"

instance Core.ToQuery GetDomainDetail where
  toQuery = Prelude.const Prelude.mempty

-- | The GetDomainDetail response includes the following elements.
--
-- /See:/ 'newGetDomainDetailResponse' smart constructor.
data GetDomainDetailResponse = GetDomainDetailResponse'
  { -- | Reserved for future use.
    dnsSec :: Prelude.Maybe Prelude.Text,
    -- | Phone number for reporting abuse.
    abuseContactPhone :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the domain registration is set to renew automatically.
    autoRenew :: Prelude.Maybe Prelude.Bool,
    -- | Email address to contact to report incorrect contact information for a
    -- domain, to report that the domain is being used to send spam, to report
    -- that someone is cybersquatting on a domain name, or report some other
    -- type of abuse.
    abuseContactEmail :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether contact information is concealed from WHOIS queries.
    -- If the value is @true@, WHOIS (\"who is\") queries return contact
    -- information either for Amazon Registrar (for .com, .net, and .org
    -- domains) or for our registrar associate, Gandi (for all other TLDs). If
    -- the value is @false@, WHOIS queries return the information that you
    -- entered for the admin contact.
    adminPrivacy :: Prelude.Maybe Prelude.Bool,
    -- | An array of domain name status codes, also known as Extensible
    -- Provisioning Protocol (EPP) status codes.
    --
    -- ICANN, the organization that maintains a central database of domain
    -- names, has developed a set of domain name status codes that tell you the
    -- status of a variety of operations on a domain name, for example,
    -- registering a domain name, transferring a domain name to another
    -- registrar, renewing the registration for a domain name, and so on. All
    -- registrars use this same set of status codes.
    --
    -- For a current list of domain name status codes and an explanation of
    -- what each code means, go to the <https://www.icann.org/ ICANN website>
    -- and search for @epp status codes@. (Search on the ICANN website; web
    -- searches sometimes return an old version of the document.)
    statusList :: Prelude.Maybe [Prelude.Text],
    -- | Reseller of the domain. Domains registered or transferred using Route 53
    -- domains will have @\"Amazon\"@ as the reseller.
    reseller :: Prelude.Maybe Prelude.Text,
    -- | Name of the registrar of the domain as identified in the registry.
    -- Domains with a .com, .net, or .org TLD are registered by Amazon
    -- Registrar. All other domains are registered by our registrar associate,
    -- Gandi. The value for domains that are registered by Gandi is
    -- @\"GANDI SAS\"@.
    registrarName :: Prelude.Maybe Prelude.Text,
    -- | Reserved for future use.
    registryDomainId :: Prelude.Maybe Prelude.Text,
    -- | The date when the domain was created as found in the response to a WHOIS
    -- query. The date and time is in Unix time format and Coordinated
    -- Universal time (UTC).
    creationDate :: Prelude.Maybe Core.POSIX,
    -- | The date when the registration for the domain is set to expire. The date
    -- and time is in Unix time format and Coordinated Universal time (UTC).
    expirationDate :: Prelude.Maybe Core.POSIX,
    -- | The fully qualified name of the WHOIS server that can answer the WHOIS
    -- query for the domain.
    whoIsServer :: Prelude.Maybe Prelude.Text,
    -- | Web address of the registrar.
    registrarUrl :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether contact information is concealed from WHOIS queries.
    -- If the value is @true@, WHOIS (\"who is\") queries return contact
    -- information either for Amazon Registrar (for .com, .net, and .org
    -- domains) or for our registrar associate, Gandi (for all other TLDs). If
    -- the value is @false@, WHOIS queries return the information that you
    -- entered for the technical contact.
    techPrivacy :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether contact information is concealed from WHOIS queries.
    -- If the value is @true@, WHOIS (\"who is\") queries return contact
    -- information either for Amazon Registrar (for .com, .net, and .org
    -- domains) or for our registrar associate, Gandi (for all other TLDs). If
    -- the value is @false@, WHOIS queries return the information that you
    -- entered for the registrant contact (domain owner).
    registrantPrivacy :: Prelude.Maybe Prelude.Bool,
    -- | The last updated date of the domain as found in the response to a WHOIS
    -- query. The date and time is in Unix time format and Coordinated
    -- Universal time (UTC).
    updatedDate :: Prelude.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of a domain.
    domainName :: Prelude.Text,
    -- | The name of the domain.
    nameservers :: [Nameserver],
    -- | Provides details about the domain administrative contact.
    adminContact :: Core.Sensitive ContactDetail,
    -- | Provides details about the domain registrant.
    registrantContact :: Core.Sensitive ContactDetail,
    -- | Provides details about the domain technical contact.
    techContact :: Core.Sensitive ContactDetail
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDomainDetailResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dnsSec', 'getDomainDetailResponse_dnsSec' - Reserved for future use.
--
-- 'abuseContactPhone', 'getDomainDetailResponse_abuseContactPhone' - Phone number for reporting abuse.
--
-- 'autoRenew', 'getDomainDetailResponse_autoRenew' - Specifies whether the domain registration is set to renew automatically.
--
-- 'abuseContactEmail', 'getDomainDetailResponse_abuseContactEmail' - Email address to contact to report incorrect contact information for a
-- domain, to report that the domain is being used to send spam, to report
-- that someone is cybersquatting on a domain name, or report some other
-- type of abuse.
--
-- 'adminPrivacy', 'getDomainDetailResponse_adminPrivacy' - Specifies whether contact information is concealed from WHOIS queries.
-- If the value is @true@, WHOIS (\"who is\") queries return contact
-- information either for Amazon Registrar (for .com, .net, and .org
-- domains) or for our registrar associate, Gandi (for all other TLDs). If
-- the value is @false@, WHOIS queries return the information that you
-- entered for the admin contact.
--
-- 'statusList', 'getDomainDetailResponse_statusList' - An array of domain name status codes, also known as Extensible
-- Provisioning Protocol (EPP) status codes.
--
-- ICANN, the organization that maintains a central database of domain
-- names, has developed a set of domain name status codes that tell you the
-- status of a variety of operations on a domain name, for example,
-- registering a domain name, transferring a domain name to another
-- registrar, renewing the registration for a domain name, and so on. All
-- registrars use this same set of status codes.
--
-- For a current list of domain name status codes and an explanation of
-- what each code means, go to the <https://www.icann.org/ ICANN website>
-- and search for @epp status codes@. (Search on the ICANN website; web
-- searches sometimes return an old version of the document.)
--
-- 'reseller', 'getDomainDetailResponse_reseller' - Reseller of the domain. Domains registered or transferred using Route 53
-- domains will have @\"Amazon\"@ as the reseller.
--
-- 'registrarName', 'getDomainDetailResponse_registrarName' - Name of the registrar of the domain as identified in the registry.
-- Domains with a .com, .net, or .org TLD are registered by Amazon
-- Registrar. All other domains are registered by our registrar associate,
-- Gandi. The value for domains that are registered by Gandi is
-- @\"GANDI SAS\"@.
--
-- 'registryDomainId', 'getDomainDetailResponse_registryDomainId' - Reserved for future use.
--
-- 'creationDate', 'getDomainDetailResponse_creationDate' - The date when the domain was created as found in the response to a WHOIS
-- query. The date and time is in Unix time format and Coordinated
-- Universal time (UTC).
--
-- 'expirationDate', 'getDomainDetailResponse_expirationDate' - The date when the registration for the domain is set to expire. The date
-- and time is in Unix time format and Coordinated Universal time (UTC).
--
-- 'whoIsServer', 'getDomainDetailResponse_whoIsServer' - The fully qualified name of the WHOIS server that can answer the WHOIS
-- query for the domain.
--
-- 'registrarUrl', 'getDomainDetailResponse_registrarUrl' - Web address of the registrar.
--
-- 'techPrivacy', 'getDomainDetailResponse_techPrivacy' - Specifies whether contact information is concealed from WHOIS queries.
-- If the value is @true@, WHOIS (\"who is\") queries return contact
-- information either for Amazon Registrar (for .com, .net, and .org
-- domains) or for our registrar associate, Gandi (for all other TLDs). If
-- the value is @false@, WHOIS queries return the information that you
-- entered for the technical contact.
--
-- 'registrantPrivacy', 'getDomainDetailResponse_registrantPrivacy' - Specifies whether contact information is concealed from WHOIS queries.
-- If the value is @true@, WHOIS (\"who is\") queries return contact
-- information either for Amazon Registrar (for .com, .net, and .org
-- domains) or for our registrar associate, Gandi (for all other TLDs). If
-- the value is @false@, WHOIS queries return the information that you
-- entered for the registrant contact (domain owner).
--
-- 'updatedDate', 'getDomainDetailResponse_updatedDate' - The last updated date of the domain as found in the response to a WHOIS
-- query. The date and time is in Unix time format and Coordinated
-- Universal time (UTC).
--
-- 'httpStatus', 'getDomainDetailResponse_httpStatus' - The response's http status code.
--
-- 'domainName', 'getDomainDetailResponse_domainName' - The name of a domain.
--
-- 'nameservers', 'getDomainDetailResponse_nameservers' - The name of the domain.
--
-- 'adminContact', 'getDomainDetailResponse_adminContact' - Provides details about the domain administrative contact.
--
-- 'registrantContact', 'getDomainDetailResponse_registrantContact' - Provides details about the domain registrant.
--
-- 'techContact', 'getDomainDetailResponse_techContact' - Provides details about the domain technical contact.
newGetDomainDetailResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'domainName'
  Prelude.Text ->
  -- | 'adminContact'
  ContactDetail ->
  -- | 'registrantContact'
  ContactDetail ->
  -- | 'techContact'
  ContactDetail ->
  GetDomainDetailResponse
newGetDomainDetailResponse
  pHttpStatus_
  pDomainName_
  pAdminContact_
  pRegistrantContact_
  pTechContact_ =
    GetDomainDetailResponse'
      { dnsSec = Prelude.Nothing,
        abuseContactPhone = Prelude.Nothing,
        autoRenew = Prelude.Nothing,
        abuseContactEmail = Prelude.Nothing,
        adminPrivacy = Prelude.Nothing,
        statusList = Prelude.Nothing,
        reseller = Prelude.Nothing,
        registrarName = Prelude.Nothing,
        registryDomainId = Prelude.Nothing,
        creationDate = Prelude.Nothing,
        expirationDate = Prelude.Nothing,
        whoIsServer = Prelude.Nothing,
        registrarUrl = Prelude.Nothing,
        techPrivacy = Prelude.Nothing,
        registrantPrivacy = Prelude.Nothing,
        updatedDate = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        domainName = pDomainName_,
        nameservers = Prelude.mempty,
        adminContact =
          Core._Sensitive Lens.# pAdminContact_,
        registrantContact =
          Core._Sensitive Lens.# pRegistrantContact_,
        techContact = Core._Sensitive Lens.# pTechContact_
      }

-- | Reserved for future use.
getDomainDetailResponse_dnsSec :: Lens.Lens' GetDomainDetailResponse (Prelude.Maybe Prelude.Text)
getDomainDetailResponse_dnsSec = Lens.lens (\GetDomainDetailResponse' {dnsSec} -> dnsSec) (\s@GetDomainDetailResponse' {} a -> s {dnsSec = a} :: GetDomainDetailResponse)

-- | Phone number for reporting abuse.
getDomainDetailResponse_abuseContactPhone :: Lens.Lens' GetDomainDetailResponse (Prelude.Maybe Prelude.Text)
getDomainDetailResponse_abuseContactPhone = Lens.lens (\GetDomainDetailResponse' {abuseContactPhone} -> abuseContactPhone) (\s@GetDomainDetailResponse' {} a -> s {abuseContactPhone = a} :: GetDomainDetailResponse)

-- | Specifies whether the domain registration is set to renew automatically.
getDomainDetailResponse_autoRenew :: Lens.Lens' GetDomainDetailResponse (Prelude.Maybe Prelude.Bool)
getDomainDetailResponse_autoRenew = Lens.lens (\GetDomainDetailResponse' {autoRenew} -> autoRenew) (\s@GetDomainDetailResponse' {} a -> s {autoRenew = a} :: GetDomainDetailResponse)

-- | Email address to contact to report incorrect contact information for a
-- domain, to report that the domain is being used to send spam, to report
-- that someone is cybersquatting on a domain name, or report some other
-- type of abuse.
getDomainDetailResponse_abuseContactEmail :: Lens.Lens' GetDomainDetailResponse (Prelude.Maybe Prelude.Text)
getDomainDetailResponse_abuseContactEmail = Lens.lens (\GetDomainDetailResponse' {abuseContactEmail} -> abuseContactEmail) (\s@GetDomainDetailResponse' {} a -> s {abuseContactEmail = a} :: GetDomainDetailResponse)

-- | Specifies whether contact information is concealed from WHOIS queries.
-- If the value is @true@, WHOIS (\"who is\") queries return contact
-- information either for Amazon Registrar (for .com, .net, and .org
-- domains) or for our registrar associate, Gandi (for all other TLDs). If
-- the value is @false@, WHOIS queries return the information that you
-- entered for the admin contact.
getDomainDetailResponse_adminPrivacy :: Lens.Lens' GetDomainDetailResponse (Prelude.Maybe Prelude.Bool)
getDomainDetailResponse_adminPrivacy = Lens.lens (\GetDomainDetailResponse' {adminPrivacy} -> adminPrivacy) (\s@GetDomainDetailResponse' {} a -> s {adminPrivacy = a} :: GetDomainDetailResponse)

-- | An array of domain name status codes, also known as Extensible
-- Provisioning Protocol (EPP) status codes.
--
-- ICANN, the organization that maintains a central database of domain
-- names, has developed a set of domain name status codes that tell you the
-- status of a variety of operations on a domain name, for example,
-- registering a domain name, transferring a domain name to another
-- registrar, renewing the registration for a domain name, and so on. All
-- registrars use this same set of status codes.
--
-- For a current list of domain name status codes and an explanation of
-- what each code means, go to the <https://www.icann.org/ ICANN website>
-- and search for @epp status codes@. (Search on the ICANN website; web
-- searches sometimes return an old version of the document.)
getDomainDetailResponse_statusList :: Lens.Lens' GetDomainDetailResponse (Prelude.Maybe [Prelude.Text])
getDomainDetailResponse_statusList = Lens.lens (\GetDomainDetailResponse' {statusList} -> statusList) (\s@GetDomainDetailResponse' {} a -> s {statusList = a} :: GetDomainDetailResponse) Prelude.. Lens.mapping Lens._Coerce

-- | Reseller of the domain. Domains registered or transferred using Route 53
-- domains will have @\"Amazon\"@ as the reseller.
getDomainDetailResponse_reseller :: Lens.Lens' GetDomainDetailResponse (Prelude.Maybe Prelude.Text)
getDomainDetailResponse_reseller = Lens.lens (\GetDomainDetailResponse' {reseller} -> reseller) (\s@GetDomainDetailResponse' {} a -> s {reseller = a} :: GetDomainDetailResponse)

-- | Name of the registrar of the domain as identified in the registry.
-- Domains with a .com, .net, or .org TLD are registered by Amazon
-- Registrar. All other domains are registered by our registrar associate,
-- Gandi. The value for domains that are registered by Gandi is
-- @\"GANDI SAS\"@.
getDomainDetailResponse_registrarName :: Lens.Lens' GetDomainDetailResponse (Prelude.Maybe Prelude.Text)
getDomainDetailResponse_registrarName = Lens.lens (\GetDomainDetailResponse' {registrarName} -> registrarName) (\s@GetDomainDetailResponse' {} a -> s {registrarName = a} :: GetDomainDetailResponse)

-- | Reserved for future use.
getDomainDetailResponse_registryDomainId :: Lens.Lens' GetDomainDetailResponse (Prelude.Maybe Prelude.Text)
getDomainDetailResponse_registryDomainId = Lens.lens (\GetDomainDetailResponse' {registryDomainId} -> registryDomainId) (\s@GetDomainDetailResponse' {} a -> s {registryDomainId = a} :: GetDomainDetailResponse)

-- | The date when the domain was created as found in the response to a WHOIS
-- query. The date and time is in Unix time format and Coordinated
-- Universal time (UTC).
getDomainDetailResponse_creationDate :: Lens.Lens' GetDomainDetailResponse (Prelude.Maybe Prelude.UTCTime)
getDomainDetailResponse_creationDate = Lens.lens (\GetDomainDetailResponse' {creationDate} -> creationDate) (\s@GetDomainDetailResponse' {} a -> s {creationDate = a} :: GetDomainDetailResponse) Prelude.. Lens.mapping Core._Time

-- | The date when the registration for the domain is set to expire. The date
-- and time is in Unix time format and Coordinated Universal time (UTC).
getDomainDetailResponse_expirationDate :: Lens.Lens' GetDomainDetailResponse (Prelude.Maybe Prelude.UTCTime)
getDomainDetailResponse_expirationDate = Lens.lens (\GetDomainDetailResponse' {expirationDate} -> expirationDate) (\s@GetDomainDetailResponse' {} a -> s {expirationDate = a} :: GetDomainDetailResponse) Prelude.. Lens.mapping Core._Time

-- | The fully qualified name of the WHOIS server that can answer the WHOIS
-- query for the domain.
getDomainDetailResponse_whoIsServer :: Lens.Lens' GetDomainDetailResponse (Prelude.Maybe Prelude.Text)
getDomainDetailResponse_whoIsServer = Lens.lens (\GetDomainDetailResponse' {whoIsServer} -> whoIsServer) (\s@GetDomainDetailResponse' {} a -> s {whoIsServer = a} :: GetDomainDetailResponse)

-- | Web address of the registrar.
getDomainDetailResponse_registrarUrl :: Lens.Lens' GetDomainDetailResponse (Prelude.Maybe Prelude.Text)
getDomainDetailResponse_registrarUrl = Lens.lens (\GetDomainDetailResponse' {registrarUrl} -> registrarUrl) (\s@GetDomainDetailResponse' {} a -> s {registrarUrl = a} :: GetDomainDetailResponse)

-- | Specifies whether contact information is concealed from WHOIS queries.
-- If the value is @true@, WHOIS (\"who is\") queries return contact
-- information either for Amazon Registrar (for .com, .net, and .org
-- domains) or for our registrar associate, Gandi (for all other TLDs). If
-- the value is @false@, WHOIS queries return the information that you
-- entered for the technical contact.
getDomainDetailResponse_techPrivacy :: Lens.Lens' GetDomainDetailResponse (Prelude.Maybe Prelude.Bool)
getDomainDetailResponse_techPrivacy = Lens.lens (\GetDomainDetailResponse' {techPrivacy} -> techPrivacy) (\s@GetDomainDetailResponse' {} a -> s {techPrivacy = a} :: GetDomainDetailResponse)

-- | Specifies whether contact information is concealed from WHOIS queries.
-- If the value is @true@, WHOIS (\"who is\") queries return contact
-- information either for Amazon Registrar (for .com, .net, and .org
-- domains) or for our registrar associate, Gandi (for all other TLDs). If
-- the value is @false@, WHOIS queries return the information that you
-- entered for the registrant contact (domain owner).
getDomainDetailResponse_registrantPrivacy :: Lens.Lens' GetDomainDetailResponse (Prelude.Maybe Prelude.Bool)
getDomainDetailResponse_registrantPrivacy = Lens.lens (\GetDomainDetailResponse' {registrantPrivacy} -> registrantPrivacy) (\s@GetDomainDetailResponse' {} a -> s {registrantPrivacy = a} :: GetDomainDetailResponse)

-- | The last updated date of the domain as found in the response to a WHOIS
-- query. The date and time is in Unix time format and Coordinated
-- Universal time (UTC).
getDomainDetailResponse_updatedDate :: Lens.Lens' GetDomainDetailResponse (Prelude.Maybe Prelude.UTCTime)
getDomainDetailResponse_updatedDate = Lens.lens (\GetDomainDetailResponse' {updatedDate} -> updatedDate) (\s@GetDomainDetailResponse' {} a -> s {updatedDate = a} :: GetDomainDetailResponse) Prelude.. Lens.mapping Core._Time

-- | The response's http status code.
getDomainDetailResponse_httpStatus :: Lens.Lens' GetDomainDetailResponse Prelude.Int
getDomainDetailResponse_httpStatus = Lens.lens (\GetDomainDetailResponse' {httpStatus} -> httpStatus) (\s@GetDomainDetailResponse' {} a -> s {httpStatus = a} :: GetDomainDetailResponse)

-- | The name of a domain.
getDomainDetailResponse_domainName :: Lens.Lens' GetDomainDetailResponse Prelude.Text
getDomainDetailResponse_domainName = Lens.lens (\GetDomainDetailResponse' {domainName} -> domainName) (\s@GetDomainDetailResponse' {} a -> s {domainName = a} :: GetDomainDetailResponse)

-- | The name of the domain.
getDomainDetailResponse_nameservers :: Lens.Lens' GetDomainDetailResponse [Nameserver]
getDomainDetailResponse_nameservers = Lens.lens (\GetDomainDetailResponse' {nameservers} -> nameservers) (\s@GetDomainDetailResponse' {} a -> s {nameservers = a} :: GetDomainDetailResponse) Prelude.. Lens._Coerce

-- | Provides details about the domain administrative contact.
getDomainDetailResponse_adminContact :: Lens.Lens' GetDomainDetailResponse ContactDetail
getDomainDetailResponse_adminContact = Lens.lens (\GetDomainDetailResponse' {adminContact} -> adminContact) (\s@GetDomainDetailResponse' {} a -> s {adminContact = a} :: GetDomainDetailResponse) Prelude.. Core._Sensitive

-- | Provides details about the domain registrant.
getDomainDetailResponse_registrantContact :: Lens.Lens' GetDomainDetailResponse ContactDetail
getDomainDetailResponse_registrantContact = Lens.lens (\GetDomainDetailResponse' {registrantContact} -> registrantContact) (\s@GetDomainDetailResponse' {} a -> s {registrantContact = a} :: GetDomainDetailResponse) Prelude.. Core._Sensitive

-- | Provides details about the domain technical contact.
getDomainDetailResponse_techContact :: Lens.Lens' GetDomainDetailResponse ContactDetail
getDomainDetailResponse_techContact = Lens.lens (\GetDomainDetailResponse' {techContact} -> techContact) (\s@GetDomainDetailResponse' {} a -> s {techContact = a} :: GetDomainDetailResponse) Prelude.. Core._Sensitive

instance Prelude.NFData GetDomainDetailResponse
