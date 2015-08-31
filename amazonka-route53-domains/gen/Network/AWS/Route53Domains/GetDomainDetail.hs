{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.GetDomainDetail
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns detailed information about the domain. The
-- domain\'s contact information is also returned as part of the output.
--
-- /See:/ <http://docs.aws.amazon.com/Route53/latest/APIReference/api-GetDomainDetail.html AWS API Reference> for GetDomainDetail.
module Network.AWS.Route53Domains.GetDomainDetail
    (
    -- * Creating a Request
      getDomainDetail
    , GetDomainDetail
    -- * Request Lenses
    , gddDomainName

    -- * Destructuring the Response
    , getDomainDetailResponse
    , GetDomainDetailResponse
    -- * Response Lenses
    , gddrsTechPrivacy
    , gddrsDNSSec
    , gddrsWhoIsServer
    , gddrsRegistryDomainId
    , gddrsRegistrantPrivacy
    , gddrsUpdatedDate
    , gddrsAdminPrivacy
    , gddrsAutoRenew
    , gddrsAbuseContactPhone
    , gddrsRegistrarURL
    , gddrsAbuseContactEmail
    , gddrsExpirationDate
    , gddrsCreationDate
    , gddrsRegistrarName
    , gddrsReseller
    , gddrsStatusList
    , gddrsResponseStatus
    , gddrsDomainName
    , gddrsNameservers
    , gddrsAdminContact
    , gddrsRegistrantContact
    , gddrsTechContact
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53Domains.Types
import           Network.AWS.Route53Domains.Types.Product

-- | The GetDomainDetail request includes the following element.
--
-- /See:/ 'getDomainDetail' smart constructor.
newtype GetDomainDetail = GetDomainDetail'
    { _gddDomainName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetDomainDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gddDomainName'
getDomainDetail
    :: Text -- ^ 'gddDomainName'
    -> GetDomainDetail
getDomainDetail pDomainName_ =
    GetDomainDetail'
    { _gddDomainName = pDomainName_
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
        type Rs GetDomainDetail = GetDomainDetailResponse
        request = postJSON route53Domains
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
                     <*> (x .?> "AutoRenew")
                     <*> (x .?> "AbuseContactPhone")
                     <*> (x .?> "RegistrarUrl")
                     <*> (x .?> "AbuseContactEmail")
                     <*> (x .?> "ExpirationDate")
                     <*> (x .?> "CreationDate")
                     <*> (x .?> "RegistrarName")
                     <*> (x .?> "Reseller")
                     <*> (x .?> "StatusList" .!@ mempty)
                     <*> (pure (fromEnum s))
                     <*> (x .:> "DomainName")
                     <*> (x .?> "Nameservers" .!@ mempty)
                     <*> (x .:> "AdminContact")
                     <*> (x .:> "RegistrantContact")
                     <*> (x .:> "TechContact"))

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
          = object
              (catMaybes [Just ("DomainName" .= _gddDomainName)])

instance ToPath GetDomainDetail where
        toPath = const "/"

instance ToQuery GetDomainDetail where
        toQuery = const mempty

-- | The GetDomainDetail response includes the following elements.
--
-- /See:/ 'getDomainDetailResponse' smart constructor.
data GetDomainDetailResponse = GetDomainDetailResponse'
    { _gddrsTechPrivacy       :: !(Maybe Bool)
    , _gddrsDNSSec            :: !(Maybe Text)
    , _gddrsWhoIsServer       :: !(Maybe Text)
    , _gddrsRegistryDomainId  :: !(Maybe Text)
    , _gddrsRegistrantPrivacy :: !(Maybe Bool)
    , _gddrsUpdatedDate       :: !(Maybe POSIX)
    , _gddrsAdminPrivacy      :: !(Maybe Bool)
    , _gddrsAutoRenew         :: !(Maybe Bool)
    , _gddrsAbuseContactPhone :: !(Maybe Text)
    , _gddrsRegistrarURL      :: !(Maybe Text)
    , _gddrsAbuseContactEmail :: !(Maybe Text)
    , _gddrsExpirationDate    :: !(Maybe POSIX)
    , _gddrsCreationDate      :: !(Maybe POSIX)
    , _gddrsRegistrarName     :: !(Maybe Text)
    , _gddrsReseller          :: !(Maybe Text)
    , _gddrsStatusList        :: !(Maybe [Text])
    , _gddrsResponseStatus    :: !Int
    , _gddrsDomainName        :: !Text
    , _gddrsNameservers       :: ![Nameserver]
    , _gddrsAdminContact      :: !(Sensitive ContactDetail)
    , _gddrsRegistrantContact :: !(Sensitive ContactDetail)
    , _gddrsTechContact       :: !(Sensitive ContactDetail)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetDomainDetailResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gddrsTechPrivacy'
--
-- * 'gddrsDNSSec'
--
-- * 'gddrsWhoIsServer'
--
-- * 'gddrsRegistryDomainId'
--
-- * 'gddrsRegistrantPrivacy'
--
-- * 'gddrsUpdatedDate'
--
-- * 'gddrsAdminPrivacy'
--
-- * 'gddrsAutoRenew'
--
-- * 'gddrsAbuseContactPhone'
--
-- * 'gddrsRegistrarURL'
--
-- * 'gddrsAbuseContactEmail'
--
-- * 'gddrsExpirationDate'
--
-- * 'gddrsCreationDate'
--
-- * 'gddrsRegistrarName'
--
-- * 'gddrsReseller'
--
-- * 'gddrsStatusList'
--
-- * 'gddrsResponseStatus'
--
-- * 'gddrsDomainName'
--
-- * 'gddrsNameservers'
--
-- * 'gddrsAdminContact'
--
-- * 'gddrsRegistrantContact'
--
-- * 'gddrsTechContact'
getDomainDetailResponse
    :: Int -- ^ 'gddrsResponseStatus'
    -> Text -- ^ 'gddrsDomainName'
    -> ContactDetail -- ^ 'gddrsAdminContact'
    -> ContactDetail -- ^ 'gddrsRegistrantContact'
    -> ContactDetail -- ^ 'gddrsTechContact'
    -> GetDomainDetailResponse
getDomainDetailResponse pResponseStatus_ pDomainName_ pAdminContact_ pRegistrantContact_ pTechContact_ =
    GetDomainDetailResponse'
    { _gddrsTechPrivacy = Nothing
    , _gddrsDNSSec = Nothing
    , _gddrsWhoIsServer = Nothing
    , _gddrsRegistryDomainId = Nothing
    , _gddrsRegistrantPrivacy = Nothing
    , _gddrsUpdatedDate = Nothing
    , _gddrsAdminPrivacy = Nothing
    , _gddrsAutoRenew = Nothing
    , _gddrsAbuseContactPhone = Nothing
    , _gddrsRegistrarURL = Nothing
    , _gddrsAbuseContactEmail = Nothing
    , _gddrsExpirationDate = Nothing
    , _gddrsCreationDate = Nothing
    , _gddrsRegistrarName = Nothing
    , _gddrsReseller = Nothing
    , _gddrsStatusList = Nothing
    , _gddrsResponseStatus = pResponseStatus_
    , _gddrsDomainName = pDomainName_
    , _gddrsNameservers = mempty
    , _gddrsAdminContact = _Sensitive # pAdminContact_
    , _gddrsRegistrantContact = _Sensitive # pRegistrantContact_
    , _gddrsTechContact = _Sensitive # pTechContact_
    }

-- | Specifies whether contact information for the tech contact is concealed
-- from WHOIS queries. If the value is 'true', WHOIS (\"who is\") queries
-- will return contact information for our registrar partner, Gandi,
-- instead of the contact information that you enter.
--
-- Type: Boolean
gddrsTechPrivacy :: Lens' GetDomainDetailResponse (Maybe Bool)
gddrsTechPrivacy = lens _gddrsTechPrivacy (\ s a -> s{_gddrsTechPrivacy = a});

-- | Reserved for future use.
gddrsDNSSec :: Lens' GetDomainDetailResponse (Maybe Text)
gddrsDNSSec = lens _gddrsDNSSec (\ s a -> s{_gddrsDNSSec = a});

-- | The fully qualified name of the WHOIS server that can answer the WHOIS
-- query for the domain.
--
-- Type: String
gddrsWhoIsServer :: Lens' GetDomainDetailResponse (Maybe Text)
gddrsWhoIsServer = lens _gddrsWhoIsServer (\ s a -> s{_gddrsWhoIsServer = a});

-- | Reserved for future use.
gddrsRegistryDomainId :: Lens' GetDomainDetailResponse (Maybe Text)
gddrsRegistryDomainId = lens _gddrsRegistryDomainId (\ s a -> s{_gddrsRegistryDomainId = a});

-- | Specifies whether contact information for the registrant contact is
-- concealed from WHOIS queries. If the value is 'true', WHOIS (\"who is\")
-- queries will return contact information for our registrar partner,
-- Gandi, instead of the contact information that you enter.
--
-- Type: Boolean
gddrsRegistrantPrivacy :: Lens' GetDomainDetailResponse (Maybe Bool)
gddrsRegistrantPrivacy = lens _gddrsRegistrantPrivacy (\ s a -> s{_gddrsRegistrantPrivacy = a});

-- | The last updated date of the domain as found in the response to a WHOIS
-- query. The date format is Unix time.
gddrsUpdatedDate :: Lens' GetDomainDetailResponse (Maybe UTCTime)
gddrsUpdatedDate = lens _gddrsUpdatedDate (\ s a -> s{_gddrsUpdatedDate = a}) . mapping _Time;

-- | Specifies whether contact information for the admin contact is concealed
-- from WHOIS queries. If the value is 'true', WHOIS (\"who is\") queries
-- will return contact information for our registrar partner, Gandi,
-- instead of the contact information that you enter.
--
-- Type: Boolean
gddrsAdminPrivacy :: Lens' GetDomainDetailResponse (Maybe Bool)
gddrsAdminPrivacy = lens _gddrsAdminPrivacy (\ s a -> s{_gddrsAdminPrivacy = a});

-- | Specifies whether the domain registration is set to renew automatically.
--
-- Type: Boolean
gddrsAutoRenew :: Lens' GetDomainDetailResponse (Maybe Bool)
gddrsAutoRenew = lens _gddrsAutoRenew (\ s a -> s{_gddrsAutoRenew = a});

-- | Phone number for reporting abuse.
--
-- Type: String
gddrsAbuseContactPhone :: Lens' GetDomainDetailResponse (Maybe Text)
gddrsAbuseContactPhone = lens _gddrsAbuseContactPhone (\ s a -> s{_gddrsAbuseContactPhone = a});

-- | Web address of the registrar.
--
-- Type: String
gddrsRegistrarURL :: Lens' GetDomainDetailResponse (Maybe Text)
gddrsRegistrarURL = lens _gddrsRegistrarURL (\ s a -> s{_gddrsRegistrarURL = a});

-- | Email address to contact to report incorrect contact information for a
-- domain, to report that the domain is being used to send spam, to report
-- that someone is cybersquatting on a domain name, or report some other
-- type of abuse.
--
-- Type: String
gddrsAbuseContactEmail :: Lens' GetDomainDetailResponse (Maybe Text)
gddrsAbuseContactEmail = lens _gddrsAbuseContactEmail (\ s a -> s{_gddrsAbuseContactEmail = a});

-- | The date when the registration for the domain is set to expire. The date
-- format is Unix time.
gddrsExpirationDate :: Lens' GetDomainDetailResponse (Maybe UTCTime)
gddrsExpirationDate = lens _gddrsExpirationDate (\ s a -> s{_gddrsExpirationDate = a}) . mapping _Time;

-- | The date when the domain was created as found in the response to a WHOIS
-- query. The date format is Unix time.
gddrsCreationDate :: Lens' GetDomainDetailResponse (Maybe UTCTime)
gddrsCreationDate = lens _gddrsCreationDate (\ s a -> s{_gddrsCreationDate = a}) . mapping _Time;

-- | Name of the registrar of the domain as identified in the registry.
-- Amazon Route 53 domains are registered by registrar Gandi. The value is
-- '\"GANDI SAS\"'.
--
-- Type: String
gddrsRegistrarName :: Lens' GetDomainDetailResponse (Maybe Text)
gddrsRegistrarName = lens _gddrsRegistrarName (\ s a -> s{_gddrsRegistrarName = a});

-- | Reseller of the domain. Domains registered or transferred using Amazon
-- Route 53 domains will have '\"Amazon\"' as the reseller.
--
-- Type: String
gddrsReseller :: Lens' GetDomainDetailResponse (Maybe Text)
gddrsReseller = lens _gddrsReseller (\ s a -> s{_gddrsReseller = a});

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
-- and search for 'epp status codes'. (Search on the ICANN website; web
-- searches sometimes return an old version of the document.)
--
-- Type: Array of String
gddrsStatusList :: Lens' GetDomainDetailResponse [Text]
gddrsStatusList = lens _gddrsStatusList (\ s a -> s{_gddrsStatusList = a}) . _Default . _Coerce;

-- | The response status code.
gddrsResponseStatus :: Lens' GetDomainDetailResponse Int
gddrsResponseStatus = lens _gddrsResponseStatus (\ s a -> s{_gddrsResponseStatus = a});

-- | The name of a domain.
--
-- Type: String
gddrsDomainName :: Lens' GetDomainDetailResponse Text
gddrsDomainName = lens _gddrsDomainName (\ s a -> s{_gddrsDomainName = a});

-- | The name of the domain.
--
-- Type: String
gddrsNameservers :: Lens' GetDomainDetailResponse [Nameserver]
gddrsNameservers = lens _gddrsNameservers (\ s a -> s{_gddrsNameservers = a}) . _Coerce;

-- | Provides details about the domain administrative contact.
--
-- Type: Complex
--
-- Children: 'FirstName', 'MiddleName', 'LastName', 'ContactType',
-- 'OrganizationName', 'AddressLine1', 'AddressLine2', 'City', 'State',
-- 'CountryCode', 'ZipCode', 'PhoneNumber', 'Email', 'Fax', 'ExtraParams'
gddrsAdminContact :: Lens' GetDomainDetailResponse ContactDetail
gddrsAdminContact = lens _gddrsAdminContact (\ s a -> s{_gddrsAdminContact = a}) . _Sensitive;

-- | Provides details about the domain registrant.
--
-- Type: Complex
--
-- Children: 'FirstName', 'MiddleName', 'LastName', 'ContactType',
-- 'OrganizationName', 'AddressLine1', 'AddressLine2', 'City', 'State',
-- 'CountryCode', 'ZipCode', 'PhoneNumber', 'Email', 'Fax', 'ExtraParams'
gddrsRegistrantContact :: Lens' GetDomainDetailResponse ContactDetail
gddrsRegistrantContact = lens _gddrsRegistrantContact (\ s a -> s{_gddrsRegistrantContact = a}) . _Sensitive;

-- | Provides details about the domain technical contact.
--
-- Type: Complex
--
-- Children: 'FirstName', 'MiddleName', 'LastName', 'ContactType',
-- 'OrganizationName', 'AddressLine1', 'AddressLine2', 'City', 'State',
-- 'CountryCode', 'ZipCode', 'PhoneNumber', 'Email', 'Fax', 'ExtraParams'
gddrsTechContact :: Lens' GetDomainDetailResponse ContactDetail
gddrsTechContact = lens _gddrsTechContact (\ s a -> s{_gddrsTechContact = a}) . _Sensitive;
