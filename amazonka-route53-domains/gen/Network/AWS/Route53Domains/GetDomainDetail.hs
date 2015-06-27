{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Route53Domains.GetDomainDetail
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | This operation returns detailed information about the domain. The
-- domain\'s contact information is also returned as part of the output.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/api-GetDomainDetail.html>
module Network.AWS.Route53Domains.GetDomainDetail
    (
    -- * Request
      GetDomainDetail
    -- ** Request constructor
    , getDomainDetail
    -- ** Request lenses
    , gddDomainName

    -- * Response
    , GetDomainDetailResponse
    -- ** Response constructor
    , getDomainDetailResponse
    -- ** Response lenses
    , gddrTechPrivacy
    , gddrDNSSec
    , gddrWhoIsServer
    , gddrRegistryDomainId
    , gddrRegistrantPrivacy
    , gddrUpdatedDate
    , gddrAdminPrivacy
    , gddrAbuseContactEmail
    , gddrRegistrarURL
    , gddrAutoRenew
    , gddrAbuseContactPhone
    , gddrExpirationDate
    , gddrCreationDate
    , gddrRegistrarName
    , gddrStatusList
    , gddrReseller
    , gddrDomainName
    , gddrNameservers
    , gddrAdminContact
    , gddrRegistrantContact
    , gddrTechContact
    , gddrStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53Domains.Types

-- | The GetDomainDetail request includes the following element.
--
-- /See:/ 'getDomainDetail' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gddDomainName'
newtype GetDomainDetail = GetDomainDetail'
    { _gddDomainName :: Text
    } deriving (Eq,Read,Show)

-- | 'GetDomainDetail' smart constructor.
getDomainDetail :: Text -> GetDomainDetail
getDomainDetail pDomainName =
    GetDomainDetail'
    { _gddDomainName = pDomainName
    }

-- | The name of a domain.
--
-- Type: String
--
-- Default: None
--
-- Constraints: The domain name can contain only the letters a through z,
-- the numbers 0 through 9, and hyphen (-). Internationalized Domain Names
-- are not supported.
--
-- Required: Yes
gddDomainName :: Lens' GetDomainDetail Text
gddDomainName = lens _gddDomainName (\ s a -> s{_gddDomainName = a});

instance AWSRequest GetDomainDetail where
        type Sv GetDomainDetail = Route53Domains
        type Rs GetDomainDetail = GetDomainDetailResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 GetDomainDetailResponse' <$>
                   (x .?> "TechPrivacy") <*> (x .?> "DnsSec") <*>
                     (x .?> "WhoIsServer")
                     <*> (x .?> "RegistryDomainId")
                     <*> (x .?> "RegistrantPrivacy")
                     <*> (x .?> "UpdatedDate")
                     <*> (x .?> "AdminPrivacy")
                     <*> (x .?> "AbuseContactEmail")
                     <*> (x .?> "RegistrarUrl")
                     <*> (x .?> "AutoRenew")
                     <*> (x .?> "AbuseContactPhone")
                     <*> (x .?> "ExpirationDate")
                     <*> (x .?> "CreationDate")
                     <*> (x .?> "RegistrarName")
                     <*> (x .?> "StatusList" .!@ mempty)
                     <*> (x .?> "Reseller")
                     <*> (x .:> "DomainName")
                     <*> (x .?> "Nameservers" .!@ mempty)
                     <*> (x .:> "AdminContact")
                     <*> (x .:> "RegistrantContact")
                     <*> (x .:> "TechContact")
                     <*> (pure (fromEnum s)))

instance ToHeaders GetDomainDetail where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Route53Domains_v20140515.GetDomainDetail" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetDomainDetail where
        toJSON GetDomainDetail'{..}
          = object ["DomainName" .= _gddDomainName]

instance ToPath GetDomainDetail where
        toPath = const "/"

instance ToQuery GetDomainDetail where
        toQuery = const mempty

-- | The GetDomainDetail response includes the following elements.
--
-- /See:/ 'getDomainDetailResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gddrTechPrivacy'
--
-- * 'gddrDNSSec'
--
-- * 'gddrWhoIsServer'
--
-- * 'gddrRegistryDomainId'
--
-- * 'gddrRegistrantPrivacy'
--
-- * 'gddrUpdatedDate'
--
-- * 'gddrAdminPrivacy'
--
-- * 'gddrAbuseContactEmail'
--
-- * 'gddrRegistrarURL'
--
-- * 'gddrAutoRenew'
--
-- * 'gddrAbuseContactPhone'
--
-- * 'gddrExpirationDate'
--
-- * 'gddrCreationDate'
--
-- * 'gddrRegistrarName'
--
-- * 'gddrStatusList'
--
-- * 'gddrReseller'
--
-- * 'gddrDomainName'
--
-- * 'gddrNameservers'
--
-- * 'gddrAdminContact'
--
-- * 'gddrRegistrantContact'
--
-- * 'gddrTechContact'
--
-- * 'gddrStatus'
data GetDomainDetailResponse = GetDomainDetailResponse'
    { _gddrTechPrivacy       :: Maybe Bool
    , _gddrDNSSec            :: Maybe Text
    , _gddrWhoIsServer       :: Maybe Text
    , _gddrRegistryDomainId  :: Maybe Text
    , _gddrRegistrantPrivacy :: Maybe Bool
    , _gddrUpdatedDate       :: Maybe POSIX
    , _gddrAdminPrivacy      :: Maybe Bool
    , _gddrAbuseContactEmail :: Maybe Text
    , _gddrRegistrarURL      :: Maybe Text
    , _gddrAutoRenew         :: Maybe Bool
    , _gddrAbuseContactPhone :: Maybe Text
    , _gddrExpirationDate    :: Maybe POSIX
    , _gddrCreationDate      :: Maybe POSIX
    , _gddrRegistrarName     :: Maybe Text
    , _gddrStatusList        :: Maybe [Text]
    , _gddrReseller          :: Maybe Text
    , _gddrDomainName        :: Text
    , _gddrNameservers       :: [Nameserver]
    , _gddrAdminContact      :: Sensitive ContactDetail
    , _gddrRegistrantContact :: Sensitive ContactDetail
    , _gddrTechContact       :: Sensitive ContactDetail
    , _gddrStatus            :: !Int
    } deriving (Eq,Read,Show)

-- | 'GetDomainDetailResponse' smart constructor.
getDomainDetailResponse :: Text -> ContactDetail -> ContactDetail -> ContactDetail -> Int -> GetDomainDetailResponse
getDomainDetailResponse pDomainName pAdminContact pRegistrantContact pTechContact pStatus =
    GetDomainDetailResponse'
    { _gddrTechPrivacy = Nothing
    , _gddrDNSSec = Nothing
    , _gddrWhoIsServer = Nothing
    , _gddrRegistryDomainId = Nothing
    , _gddrRegistrantPrivacy = Nothing
    , _gddrUpdatedDate = Nothing
    , _gddrAdminPrivacy = Nothing
    , _gddrAbuseContactEmail = Nothing
    , _gddrRegistrarURL = Nothing
    , _gddrAutoRenew = Nothing
    , _gddrAbuseContactPhone = Nothing
    , _gddrExpirationDate = Nothing
    , _gddrCreationDate = Nothing
    , _gddrRegistrarName = Nothing
    , _gddrStatusList = Nothing
    , _gddrReseller = Nothing
    , _gddrDomainName = pDomainName
    , _gddrNameservers = mempty
    , _gddrAdminContact = _Sensitive # pAdminContact
    , _gddrRegistrantContact = _Sensitive # pRegistrantContact
    , _gddrTechContact = _Sensitive # pTechContact
    , _gddrStatus = pStatus
    }

-- | Specifies whether contact information for the tech contact is concealed
-- from WHOIS queries. If the value is @true@, WHOIS (\"who is\") queries
-- will return contact information for our registrar partner, Gandi,
-- instead of the contact information that you enter.
--
-- Type: Boolean
gddrTechPrivacy :: Lens' GetDomainDetailResponse (Maybe Bool)
gddrTechPrivacy = lens _gddrTechPrivacy (\ s a -> s{_gddrTechPrivacy = a});

-- | Reserved for future use.
gddrDNSSec :: Lens' GetDomainDetailResponse (Maybe Text)
gddrDNSSec = lens _gddrDNSSec (\ s a -> s{_gddrDNSSec = a});

-- | The fully qualified name of the WHOIS server that can answer the WHOIS
-- query for the domain.
--
-- Type: String
gddrWhoIsServer :: Lens' GetDomainDetailResponse (Maybe Text)
gddrWhoIsServer = lens _gddrWhoIsServer (\ s a -> s{_gddrWhoIsServer = a});

-- | Reserved for future use.
gddrRegistryDomainId :: Lens' GetDomainDetailResponse (Maybe Text)
gddrRegistryDomainId = lens _gddrRegistryDomainId (\ s a -> s{_gddrRegistryDomainId = a});

-- | Specifies whether contact information for the registrant contact is
-- concealed from WHOIS queries. If the value is @true@, WHOIS (\"who is\")
-- queries will return contact information for our registrar partner,
-- Gandi, instead of the contact information that you enter.
--
-- Type: Boolean
gddrRegistrantPrivacy :: Lens' GetDomainDetailResponse (Maybe Bool)
gddrRegistrantPrivacy = lens _gddrRegistrantPrivacy (\ s a -> s{_gddrRegistrantPrivacy = a});

-- | The last updated date of the domain as found in the response to a WHOIS
-- query. The date format is Unix time.
gddrUpdatedDate :: Lens' GetDomainDetailResponse (Maybe UTCTime)
gddrUpdatedDate = lens _gddrUpdatedDate (\ s a -> s{_gddrUpdatedDate = a}) . mapping _Time;

-- | Specifies whether contact information for the admin contact is concealed
-- from WHOIS queries. If the value is @true@, WHOIS (\"who is\") queries
-- will return contact information for our registrar partner, Gandi,
-- instead of the contact information that you enter.
--
-- Type: Boolean
gddrAdminPrivacy :: Lens' GetDomainDetailResponse (Maybe Bool)
gddrAdminPrivacy = lens _gddrAdminPrivacy (\ s a -> s{_gddrAdminPrivacy = a});

-- | Email address to contact to report incorrect contact information for a
-- domain, to report that the domain is being used to send spam, to report
-- that someone is cybersquatting on a domain name, or report some other
-- type of abuse.
--
-- Type: String
gddrAbuseContactEmail :: Lens' GetDomainDetailResponse (Maybe Text)
gddrAbuseContactEmail = lens _gddrAbuseContactEmail (\ s a -> s{_gddrAbuseContactEmail = a});

-- | Web address of the registrar.
--
-- Type: String
gddrRegistrarURL :: Lens' GetDomainDetailResponse (Maybe Text)
gddrRegistrarURL = lens _gddrRegistrarURL (\ s a -> s{_gddrRegistrarURL = a});

-- | Specifies whether the domain registration is set to renew automatically.
--
-- Type: Boolean
gddrAutoRenew :: Lens' GetDomainDetailResponse (Maybe Bool)
gddrAutoRenew = lens _gddrAutoRenew (\ s a -> s{_gddrAutoRenew = a});

-- | Phone number for reporting abuse.
--
-- Type: String
gddrAbuseContactPhone :: Lens' GetDomainDetailResponse (Maybe Text)
gddrAbuseContactPhone = lens _gddrAbuseContactPhone (\ s a -> s{_gddrAbuseContactPhone = a});

-- | The date when the registration for the domain is set to expire. The date
-- format is Unix time.
gddrExpirationDate :: Lens' GetDomainDetailResponse (Maybe UTCTime)
gddrExpirationDate = lens _gddrExpirationDate (\ s a -> s{_gddrExpirationDate = a}) . mapping _Time;

-- | The date when the domain was created as found in the response to a WHOIS
-- query. The date format is Unix time.
gddrCreationDate :: Lens' GetDomainDetailResponse (Maybe UTCTime)
gddrCreationDate = lens _gddrCreationDate (\ s a -> s{_gddrCreationDate = a}) . mapping _Time;

-- | Name of the registrar of the domain as identified in the registry.
-- Amazon Route 53 domains are registered by registrar Gandi. The value is
-- @\"GANDI SAS\"@.
--
-- Type: String
gddrRegistrarName :: Lens' GetDomainDetailResponse (Maybe Text)
gddrRegistrarName = lens _gddrRegistrarName (\ s a -> s{_gddrRegistrarName = a});

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
--
-- Type: Array of String
gddrStatusList :: Lens' GetDomainDetailResponse [Text]
gddrStatusList = lens _gddrStatusList (\ s a -> s{_gddrStatusList = a}) . _Default;

-- | Reseller of the domain. Domains registered or transferred using Amazon
-- Route 53 domains will have @\"Amazon\"@ as the reseller.
--
-- Type: String
gddrReseller :: Lens' GetDomainDetailResponse (Maybe Text)
gddrReseller = lens _gddrReseller (\ s a -> s{_gddrReseller = a});

-- | The name of a domain.
--
-- Type: String
gddrDomainName :: Lens' GetDomainDetailResponse Text
gddrDomainName = lens _gddrDomainName (\ s a -> s{_gddrDomainName = a});

-- | The name of the domain.
--
-- Type: String
gddrNameservers :: Lens' GetDomainDetailResponse [Nameserver]
gddrNameservers = lens _gddrNameservers (\ s a -> s{_gddrNameservers = a});

-- | Provides details about the domain administrative contact.
--
-- Type: Complex
--
-- Children: @FirstName@, @MiddleName@, @LastName@, @ContactType@,
-- @OrganizationName@, @AddressLine1@, @AddressLine2@, @City@, @State@,
-- @CountryCode@, @ZipCode@, @PhoneNumber@, @Email@, @Fax@, @ExtraParams@
gddrAdminContact :: Lens' GetDomainDetailResponse ContactDetail
gddrAdminContact = lens _gddrAdminContact (\ s a -> s{_gddrAdminContact = a}) . _Sensitive;

-- | Provides details about the domain registrant.
--
-- Type: Complex
--
-- Children: @FirstName@, @MiddleName@, @LastName@, @ContactType@,
-- @OrganizationName@, @AddressLine1@, @AddressLine2@, @City@, @State@,
-- @CountryCode@, @ZipCode@, @PhoneNumber@, @Email@, @Fax@, @ExtraParams@
gddrRegistrantContact :: Lens' GetDomainDetailResponse ContactDetail
gddrRegistrantContact = lens _gddrRegistrantContact (\ s a -> s{_gddrRegistrantContact = a}) . _Sensitive;

-- | Provides details about the domain technical contact.
--
-- Type: Complex
--
-- Children: @FirstName@, @MiddleName@, @LastName@, @ContactType@,
-- @OrganizationName@, @AddressLine1@, @AddressLine2@, @City@, @State@,
-- @CountryCode@, @ZipCode@, @PhoneNumber@, @Email@, @Fax@, @ExtraParams@
gddrTechContact :: Lens' GetDomainDetailResponse ContactDetail
gddrTechContact = lens _gddrTechContact (\ s a -> s{_gddrTechContact = a}) . _Sensitive;

-- | FIXME: Undocumented member.
gddrStatus :: Lens' GetDomainDetailResponse Int
gddrStatus = lens _gddrStatus (\ s a -> s{_gddrStatus = a});
