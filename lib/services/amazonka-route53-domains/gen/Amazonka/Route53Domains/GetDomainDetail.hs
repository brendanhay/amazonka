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
-- Module      : Amazonka.Route53Domains.GetDomainDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns detailed information about a specified domain
-- that is associated with the current Amazon Web Services account. Contact
-- information for the domain is also returned as part of the output.
module Amazonka.Route53Domains.GetDomainDetail
  ( -- * Creating a Request
    GetDomainDetail (..),
    newGetDomainDetail,

    -- * Request Lenses
    getDomainDetail_domainName,

    -- * Destructuring the Response
    GetDomainDetailResponse (..),
    newGetDomainDetailResponse,

    -- * Response Lenses
    getDomainDetailResponse_abuseContactEmail,
    getDomainDetailResponse_abuseContactPhone,
    getDomainDetailResponse_adminPrivacy,
    getDomainDetailResponse_autoRenew,
    getDomainDetailResponse_creationDate,
    getDomainDetailResponse_dnsSec,
    getDomainDetailResponse_expirationDate,
    getDomainDetailResponse_registrantPrivacy,
    getDomainDetailResponse_registrarName,
    getDomainDetailResponse_registrarUrl,
    getDomainDetailResponse_registryDomainId,
    getDomainDetailResponse_reseller,
    getDomainDetailResponse_statusList,
    getDomainDetailResponse_techPrivacy,
    getDomainDetailResponse_updatedDate,
    getDomainDetailResponse_whoIsServer,
    getDomainDetailResponse_httpStatus,
    getDomainDetailResponse_domainName,
    getDomainDetailResponse_nameservers,
    getDomainDetailResponse_adminContact,
    getDomainDetailResponse_registrantContact,
    getDomainDetailResponse_techContact,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Domains.Types

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDomainDetailResponse'
            Prelude.<$> (x Data..?> "AbuseContactEmail")
            Prelude.<*> (x Data..?> "AbuseContactPhone")
            Prelude.<*> (x Data..?> "AdminPrivacy")
            Prelude.<*> (x Data..?> "AutoRenew")
            Prelude.<*> (x Data..?> "CreationDate")
            Prelude.<*> (x Data..?> "DnsSec")
            Prelude.<*> (x Data..?> "ExpirationDate")
            Prelude.<*> (x Data..?> "RegistrantPrivacy")
            Prelude.<*> (x Data..?> "RegistrarName")
            Prelude.<*> (x Data..?> "RegistrarUrl")
            Prelude.<*> (x Data..?> "RegistryDomainId")
            Prelude.<*> (x Data..?> "Reseller")
            Prelude.<*> (x Data..?> "StatusList" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "TechPrivacy")
            Prelude.<*> (x Data..?> "UpdatedDate")
            Prelude.<*> (x Data..?> "WhoIsServer")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "DomainName")
            Prelude.<*> (x Data..?> "Nameservers" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..:> "AdminContact")
            Prelude.<*> (x Data..:> "RegistrantContact")
            Prelude.<*> (x Data..:> "TechContact")
      )

instance Prelude.Hashable GetDomainDetail where
  hashWithSalt _salt GetDomainDetail' {..} =
    _salt `Prelude.hashWithSalt` domainName

instance Prelude.NFData GetDomainDetail where
  rnf GetDomainDetail' {..} = Prelude.rnf domainName

instance Data.ToHeaders GetDomainDetail where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Domains_v20140515.GetDomainDetail" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetDomainDetail where
  toJSON GetDomainDetail' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("DomainName" Data..= domainName)]
      )

instance Data.ToPath GetDomainDetail where
  toPath = Prelude.const "/"

instance Data.ToQuery GetDomainDetail where
  toQuery = Prelude.const Prelude.mempty

-- | The GetDomainDetail response includes the following elements.
--
-- /See:/ 'newGetDomainDetailResponse' smart constructor.
data GetDomainDetailResponse = GetDomainDetailResponse'
  { -- | Email address to contact to report incorrect contact information for a
    -- domain, to report that the domain is being used to send spam, to report
    -- that someone is cybersquatting on a domain name, or report some other
    -- type of abuse.
    abuseContactEmail :: Prelude.Maybe Prelude.Text,
    -- | Phone number for reporting abuse.
    abuseContactPhone :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether contact information is concealed from WHOIS queries.
    -- If the value is @true@, WHOIS (\"who is\") queries return contact
    -- information either for Amazon Registrar (for .com, .net, and .org
    -- domains) or for our registrar associate, Gandi (for all other TLDs). If
    -- the value is @false@, WHOIS queries return the information that you
    -- entered for the admin contact.
    adminPrivacy :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether the domain registration is set to renew automatically.
    autoRenew :: Prelude.Maybe Prelude.Bool,
    -- | The date when the domain was created as found in the response to a WHOIS
    -- query. The date and time is in Unix time format and Coordinated
    -- Universal time (UTC).
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | Deprecated.
    dnsSec :: Prelude.Maybe Prelude.Text,
    -- | The date when the registration for the domain is set to expire. The date
    -- and time is in Unix time format and Coordinated Universal time (UTC).
    expirationDate :: Prelude.Maybe Data.POSIX,
    -- | Specifies whether contact information is concealed from WHOIS queries.
    -- If the value is @true@, WHOIS (\"who is\") queries return contact
    -- information either for Amazon Registrar (for .com, .net, and .org
    -- domains) or for our registrar associate, Gandi (for all other TLDs). If
    -- the value is @false@, WHOIS queries return the information that you
    -- entered for the registrant contact (domain owner).
    registrantPrivacy :: Prelude.Maybe Prelude.Bool,
    -- | Name of the registrar of the domain as identified in the registry.
    -- Domains with a .com, .net, or .org TLD are registered by Amazon
    -- Registrar. All other domains are registered by our registrar associate,
    -- Gandi. The value for domains that are registered by Gandi is
    -- @\"GANDI SAS\"@.
    registrarName :: Prelude.Maybe Prelude.Text,
    -- | Web address of the registrar.
    registrarUrl :: Prelude.Maybe Prelude.Text,
    -- | Reserved for future use.
    registryDomainId :: Prelude.Maybe Prelude.Text,
    -- | Reseller of the domain. Domains registered or transferred using Route 53
    -- domains will have @\"Amazon\"@ as the reseller.
    reseller :: Prelude.Maybe Prelude.Text,
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
    -- | Specifies whether contact information is concealed from WHOIS queries.
    -- If the value is @true@, WHOIS (\"who is\") queries return contact
    -- information either for Amazon Registrar (for .com, .net, and .org
    -- domains) or for our registrar associate, Gandi (for all other TLDs). If
    -- the value is @false@, WHOIS queries return the information that you
    -- entered for the technical contact.
    techPrivacy :: Prelude.Maybe Prelude.Bool,
    -- | The last updated date of the domain as found in the response to a WHOIS
    -- query. The date and time is in Unix time format and Coordinated
    -- Universal time (UTC).
    updatedDate :: Prelude.Maybe Data.POSIX,
    -- | The fully qualified name of the WHOIS server that can answer the WHOIS
    -- query for the domain.
    whoIsServer :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of a domain.
    domainName :: Prelude.Text,
    -- | The name of the domain.
    nameservers :: [Nameserver],
    -- | Provides details about the domain administrative contact.
    adminContact :: Data.Sensitive ContactDetail,
    -- | Provides details about the domain registrant.
    registrantContact :: Data.Sensitive ContactDetail,
    -- | Provides details about the domain technical contact.
    techContact :: Data.Sensitive ContactDetail
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
-- 'abuseContactEmail', 'getDomainDetailResponse_abuseContactEmail' - Email address to contact to report incorrect contact information for a
-- domain, to report that the domain is being used to send spam, to report
-- that someone is cybersquatting on a domain name, or report some other
-- type of abuse.
--
-- 'abuseContactPhone', 'getDomainDetailResponse_abuseContactPhone' - Phone number for reporting abuse.
--
-- 'adminPrivacy', 'getDomainDetailResponse_adminPrivacy' - Specifies whether contact information is concealed from WHOIS queries.
-- If the value is @true@, WHOIS (\"who is\") queries return contact
-- information either for Amazon Registrar (for .com, .net, and .org
-- domains) or for our registrar associate, Gandi (for all other TLDs). If
-- the value is @false@, WHOIS queries return the information that you
-- entered for the admin contact.
--
-- 'autoRenew', 'getDomainDetailResponse_autoRenew' - Specifies whether the domain registration is set to renew automatically.
--
-- 'creationDate', 'getDomainDetailResponse_creationDate' - The date when the domain was created as found in the response to a WHOIS
-- query. The date and time is in Unix time format and Coordinated
-- Universal time (UTC).
--
-- 'dnsSec', 'getDomainDetailResponse_dnsSec' - Deprecated.
--
-- 'expirationDate', 'getDomainDetailResponse_expirationDate' - The date when the registration for the domain is set to expire. The date
-- and time is in Unix time format and Coordinated Universal time (UTC).
--
-- 'registrantPrivacy', 'getDomainDetailResponse_registrantPrivacy' - Specifies whether contact information is concealed from WHOIS queries.
-- If the value is @true@, WHOIS (\"who is\") queries return contact
-- information either for Amazon Registrar (for .com, .net, and .org
-- domains) or for our registrar associate, Gandi (for all other TLDs). If
-- the value is @false@, WHOIS queries return the information that you
-- entered for the registrant contact (domain owner).
--
-- 'registrarName', 'getDomainDetailResponse_registrarName' - Name of the registrar of the domain as identified in the registry.
-- Domains with a .com, .net, or .org TLD are registered by Amazon
-- Registrar. All other domains are registered by our registrar associate,
-- Gandi. The value for domains that are registered by Gandi is
-- @\"GANDI SAS\"@.
--
-- 'registrarUrl', 'getDomainDetailResponse_registrarUrl' - Web address of the registrar.
--
-- 'registryDomainId', 'getDomainDetailResponse_registryDomainId' - Reserved for future use.
--
-- 'reseller', 'getDomainDetailResponse_reseller' - Reseller of the domain. Domains registered or transferred using Route 53
-- domains will have @\"Amazon\"@ as the reseller.
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
-- 'techPrivacy', 'getDomainDetailResponse_techPrivacy' - Specifies whether contact information is concealed from WHOIS queries.
-- If the value is @true@, WHOIS (\"who is\") queries return contact
-- information either for Amazon Registrar (for .com, .net, and .org
-- domains) or for our registrar associate, Gandi (for all other TLDs). If
-- the value is @false@, WHOIS queries return the information that you
-- entered for the technical contact.
--
-- 'updatedDate', 'getDomainDetailResponse_updatedDate' - The last updated date of the domain as found in the response to a WHOIS
-- query. The date and time is in Unix time format and Coordinated
-- Universal time (UTC).
--
-- 'whoIsServer', 'getDomainDetailResponse_whoIsServer' - The fully qualified name of the WHOIS server that can answer the WHOIS
-- query for the domain.
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
      { abuseContactEmail =
          Prelude.Nothing,
        abuseContactPhone = Prelude.Nothing,
        adminPrivacy = Prelude.Nothing,
        autoRenew = Prelude.Nothing,
        creationDate = Prelude.Nothing,
        dnsSec = Prelude.Nothing,
        expirationDate = Prelude.Nothing,
        registrantPrivacy = Prelude.Nothing,
        registrarName = Prelude.Nothing,
        registrarUrl = Prelude.Nothing,
        registryDomainId = Prelude.Nothing,
        reseller = Prelude.Nothing,
        statusList = Prelude.Nothing,
        techPrivacy = Prelude.Nothing,
        updatedDate = Prelude.Nothing,
        whoIsServer = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        domainName = pDomainName_,
        nameservers = Prelude.mempty,
        adminContact =
          Data._Sensitive Lens.# pAdminContact_,
        registrantContact =
          Data._Sensitive Lens.# pRegistrantContact_,
        techContact = Data._Sensitive Lens.# pTechContact_
      }

-- | Email address to contact to report incorrect contact information for a
-- domain, to report that the domain is being used to send spam, to report
-- that someone is cybersquatting on a domain name, or report some other
-- type of abuse.
getDomainDetailResponse_abuseContactEmail :: Lens.Lens' GetDomainDetailResponse (Prelude.Maybe Prelude.Text)
getDomainDetailResponse_abuseContactEmail = Lens.lens (\GetDomainDetailResponse' {abuseContactEmail} -> abuseContactEmail) (\s@GetDomainDetailResponse' {} a -> s {abuseContactEmail = a} :: GetDomainDetailResponse)

-- | Phone number for reporting abuse.
getDomainDetailResponse_abuseContactPhone :: Lens.Lens' GetDomainDetailResponse (Prelude.Maybe Prelude.Text)
getDomainDetailResponse_abuseContactPhone = Lens.lens (\GetDomainDetailResponse' {abuseContactPhone} -> abuseContactPhone) (\s@GetDomainDetailResponse' {} a -> s {abuseContactPhone = a} :: GetDomainDetailResponse)

-- | Specifies whether contact information is concealed from WHOIS queries.
-- If the value is @true@, WHOIS (\"who is\") queries return contact
-- information either for Amazon Registrar (for .com, .net, and .org
-- domains) or for our registrar associate, Gandi (for all other TLDs). If
-- the value is @false@, WHOIS queries return the information that you
-- entered for the admin contact.
getDomainDetailResponse_adminPrivacy :: Lens.Lens' GetDomainDetailResponse (Prelude.Maybe Prelude.Bool)
getDomainDetailResponse_adminPrivacy = Lens.lens (\GetDomainDetailResponse' {adminPrivacy} -> adminPrivacy) (\s@GetDomainDetailResponse' {} a -> s {adminPrivacy = a} :: GetDomainDetailResponse)

-- | Specifies whether the domain registration is set to renew automatically.
getDomainDetailResponse_autoRenew :: Lens.Lens' GetDomainDetailResponse (Prelude.Maybe Prelude.Bool)
getDomainDetailResponse_autoRenew = Lens.lens (\GetDomainDetailResponse' {autoRenew} -> autoRenew) (\s@GetDomainDetailResponse' {} a -> s {autoRenew = a} :: GetDomainDetailResponse)

-- | The date when the domain was created as found in the response to a WHOIS
-- query. The date and time is in Unix time format and Coordinated
-- Universal time (UTC).
getDomainDetailResponse_creationDate :: Lens.Lens' GetDomainDetailResponse (Prelude.Maybe Prelude.UTCTime)
getDomainDetailResponse_creationDate = Lens.lens (\GetDomainDetailResponse' {creationDate} -> creationDate) (\s@GetDomainDetailResponse' {} a -> s {creationDate = a} :: GetDomainDetailResponse) Prelude.. Lens.mapping Data._Time

-- | Deprecated.
getDomainDetailResponse_dnsSec :: Lens.Lens' GetDomainDetailResponse (Prelude.Maybe Prelude.Text)
getDomainDetailResponse_dnsSec = Lens.lens (\GetDomainDetailResponse' {dnsSec} -> dnsSec) (\s@GetDomainDetailResponse' {} a -> s {dnsSec = a} :: GetDomainDetailResponse)

-- | The date when the registration for the domain is set to expire. The date
-- and time is in Unix time format and Coordinated Universal time (UTC).
getDomainDetailResponse_expirationDate :: Lens.Lens' GetDomainDetailResponse (Prelude.Maybe Prelude.UTCTime)
getDomainDetailResponse_expirationDate = Lens.lens (\GetDomainDetailResponse' {expirationDate} -> expirationDate) (\s@GetDomainDetailResponse' {} a -> s {expirationDate = a} :: GetDomainDetailResponse) Prelude.. Lens.mapping Data._Time

-- | Specifies whether contact information is concealed from WHOIS queries.
-- If the value is @true@, WHOIS (\"who is\") queries return contact
-- information either for Amazon Registrar (for .com, .net, and .org
-- domains) or for our registrar associate, Gandi (for all other TLDs). If
-- the value is @false@, WHOIS queries return the information that you
-- entered for the registrant contact (domain owner).
getDomainDetailResponse_registrantPrivacy :: Lens.Lens' GetDomainDetailResponse (Prelude.Maybe Prelude.Bool)
getDomainDetailResponse_registrantPrivacy = Lens.lens (\GetDomainDetailResponse' {registrantPrivacy} -> registrantPrivacy) (\s@GetDomainDetailResponse' {} a -> s {registrantPrivacy = a} :: GetDomainDetailResponse)

-- | Name of the registrar of the domain as identified in the registry.
-- Domains with a .com, .net, or .org TLD are registered by Amazon
-- Registrar. All other domains are registered by our registrar associate,
-- Gandi. The value for domains that are registered by Gandi is
-- @\"GANDI SAS\"@.
getDomainDetailResponse_registrarName :: Lens.Lens' GetDomainDetailResponse (Prelude.Maybe Prelude.Text)
getDomainDetailResponse_registrarName = Lens.lens (\GetDomainDetailResponse' {registrarName} -> registrarName) (\s@GetDomainDetailResponse' {} a -> s {registrarName = a} :: GetDomainDetailResponse)

-- | Web address of the registrar.
getDomainDetailResponse_registrarUrl :: Lens.Lens' GetDomainDetailResponse (Prelude.Maybe Prelude.Text)
getDomainDetailResponse_registrarUrl = Lens.lens (\GetDomainDetailResponse' {registrarUrl} -> registrarUrl) (\s@GetDomainDetailResponse' {} a -> s {registrarUrl = a} :: GetDomainDetailResponse)

-- | Reserved for future use.
getDomainDetailResponse_registryDomainId :: Lens.Lens' GetDomainDetailResponse (Prelude.Maybe Prelude.Text)
getDomainDetailResponse_registryDomainId = Lens.lens (\GetDomainDetailResponse' {registryDomainId} -> registryDomainId) (\s@GetDomainDetailResponse' {} a -> s {registryDomainId = a} :: GetDomainDetailResponse)

-- | Reseller of the domain. Domains registered or transferred using Route 53
-- domains will have @\"Amazon\"@ as the reseller.
getDomainDetailResponse_reseller :: Lens.Lens' GetDomainDetailResponse (Prelude.Maybe Prelude.Text)
getDomainDetailResponse_reseller = Lens.lens (\GetDomainDetailResponse' {reseller} -> reseller) (\s@GetDomainDetailResponse' {} a -> s {reseller = a} :: GetDomainDetailResponse)

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
getDomainDetailResponse_statusList = Lens.lens (\GetDomainDetailResponse' {statusList} -> statusList) (\s@GetDomainDetailResponse' {} a -> s {statusList = a} :: GetDomainDetailResponse) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether contact information is concealed from WHOIS queries.
-- If the value is @true@, WHOIS (\"who is\") queries return contact
-- information either for Amazon Registrar (for .com, .net, and .org
-- domains) or for our registrar associate, Gandi (for all other TLDs). If
-- the value is @false@, WHOIS queries return the information that you
-- entered for the technical contact.
getDomainDetailResponse_techPrivacy :: Lens.Lens' GetDomainDetailResponse (Prelude.Maybe Prelude.Bool)
getDomainDetailResponse_techPrivacy = Lens.lens (\GetDomainDetailResponse' {techPrivacy} -> techPrivacy) (\s@GetDomainDetailResponse' {} a -> s {techPrivacy = a} :: GetDomainDetailResponse)

-- | The last updated date of the domain as found in the response to a WHOIS
-- query. The date and time is in Unix time format and Coordinated
-- Universal time (UTC).
getDomainDetailResponse_updatedDate :: Lens.Lens' GetDomainDetailResponse (Prelude.Maybe Prelude.UTCTime)
getDomainDetailResponse_updatedDate = Lens.lens (\GetDomainDetailResponse' {updatedDate} -> updatedDate) (\s@GetDomainDetailResponse' {} a -> s {updatedDate = a} :: GetDomainDetailResponse) Prelude.. Lens.mapping Data._Time

-- | The fully qualified name of the WHOIS server that can answer the WHOIS
-- query for the domain.
getDomainDetailResponse_whoIsServer :: Lens.Lens' GetDomainDetailResponse (Prelude.Maybe Prelude.Text)
getDomainDetailResponse_whoIsServer = Lens.lens (\GetDomainDetailResponse' {whoIsServer} -> whoIsServer) (\s@GetDomainDetailResponse' {} a -> s {whoIsServer = a} :: GetDomainDetailResponse)

-- | The response's http status code.
getDomainDetailResponse_httpStatus :: Lens.Lens' GetDomainDetailResponse Prelude.Int
getDomainDetailResponse_httpStatus = Lens.lens (\GetDomainDetailResponse' {httpStatus} -> httpStatus) (\s@GetDomainDetailResponse' {} a -> s {httpStatus = a} :: GetDomainDetailResponse)

-- | The name of a domain.
getDomainDetailResponse_domainName :: Lens.Lens' GetDomainDetailResponse Prelude.Text
getDomainDetailResponse_domainName = Lens.lens (\GetDomainDetailResponse' {domainName} -> domainName) (\s@GetDomainDetailResponse' {} a -> s {domainName = a} :: GetDomainDetailResponse)

-- | The name of the domain.
getDomainDetailResponse_nameservers :: Lens.Lens' GetDomainDetailResponse [Nameserver]
getDomainDetailResponse_nameservers = Lens.lens (\GetDomainDetailResponse' {nameservers} -> nameservers) (\s@GetDomainDetailResponse' {} a -> s {nameservers = a} :: GetDomainDetailResponse) Prelude.. Lens.coerced

-- | Provides details about the domain administrative contact.
getDomainDetailResponse_adminContact :: Lens.Lens' GetDomainDetailResponse ContactDetail
getDomainDetailResponse_adminContact = Lens.lens (\GetDomainDetailResponse' {adminContact} -> adminContact) (\s@GetDomainDetailResponse' {} a -> s {adminContact = a} :: GetDomainDetailResponse) Prelude.. Data._Sensitive

-- | Provides details about the domain registrant.
getDomainDetailResponse_registrantContact :: Lens.Lens' GetDomainDetailResponse ContactDetail
getDomainDetailResponse_registrantContact = Lens.lens (\GetDomainDetailResponse' {registrantContact} -> registrantContact) (\s@GetDomainDetailResponse' {} a -> s {registrantContact = a} :: GetDomainDetailResponse) Prelude.. Data._Sensitive

-- | Provides details about the domain technical contact.
getDomainDetailResponse_techContact :: Lens.Lens' GetDomainDetailResponse ContactDetail
getDomainDetailResponse_techContact = Lens.lens (\GetDomainDetailResponse' {techContact} -> techContact) (\s@GetDomainDetailResponse' {} a -> s {techContact = a} :: GetDomainDetailResponse) Prelude.. Data._Sensitive

instance Prelude.NFData GetDomainDetailResponse where
  rnf GetDomainDetailResponse' {..} =
    Prelude.rnf abuseContactEmail
      `Prelude.seq` Prelude.rnf abuseContactPhone
      `Prelude.seq` Prelude.rnf adminPrivacy
      `Prelude.seq` Prelude.rnf autoRenew
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf dnsSec
      `Prelude.seq` Prelude.rnf expirationDate
      `Prelude.seq` Prelude.rnf registrantPrivacy
      `Prelude.seq` Prelude.rnf registrarName
      `Prelude.seq` Prelude.rnf registrarUrl
      `Prelude.seq` Prelude.rnf registryDomainId
      `Prelude.seq` Prelude.rnf reseller
      `Prelude.seq` Prelude.rnf statusList
      `Prelude.seq` Prelude.rnf techPrivacy
      `Prelude.seq` Prelude.rnf updatedDate
      `Prelude.seq` Prelude.rnf whoIsServer
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf nameservers
      `Prelude.seq` Prelude.rnf adminContact
      `Prelude.seq` Prelude.rnf
        registrantContact
      `Prelude.seq` Prelude.rnf techContact
