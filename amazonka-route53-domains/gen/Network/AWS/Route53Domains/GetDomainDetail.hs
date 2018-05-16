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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns detailed information about a specified domain that is associated with the current AWS account. Contact information for the domain is also returned as part of the output.
--
--
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

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53Domains.Types
import Network.AWS.Route53Domains.Types.Product

-- | The GetDomainDetail request includes the following element.
--
--
--
-- /See:/ 'getDomainDetail' smart constructor.
newtype GetDomainDetail = GetDomainDetail'
  { _gddDomainName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDomainDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gddDomainName' - The name of the domain that you want to get detailed information about.
getDomainDetail
    :: Text -- ^ 'gddDomainName'
    -> GetDomainDetail
getDomainDetail pDomainName_ = GetDomainDetail' {_gddDomainName = pDomainName_}


-- | The name of the domain that you want to get detailed information about.
gddDomainName :: Lens' GetDomainDetail Text
gddDomainName = lens _gddDomainName (\ s a -> s{_gddDomainName = a})

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

instance Hashable GetDomainDetail where

instance NFData GetDomainDetail where

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
--
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
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDomainDetailResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gddrsTechPrivacy' - Specifies whether contact information is concealed from WHOIS queries. If the value is @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If the value is @false@ , WHOIS queries return the information that you entered for the technical contact.
--
-- * 'gddrsDNSSec' - Reserved for future use.
--
-- * 'gddrsWhoIsServer' - The fully qualified name of the WHOIS server that can answer the WHOIS query for the domain.
--
-- * 'gddrsRegistryDomainId' - Reserved for future use.
--
-- * 'gddrsRegistrantPrivacy' - Specifies whether contact information is concealed from WHOIS queries. If the value is @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If the value is @false@ , WHOIS queries return the information that you entered for the registrant contact (domain owner).
--
-- * 'gddrsUpdatedDate' - The last updated date of the domain as found in the response to a WHOIS query. The date and time is in Coordinated Universal time (UTC).
--
-- * 'gddrsAdminPrivacy' - Specifies whether contact information is concealed from WHOIS queries. If the value is @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If the value is @false@ , WHOIS queries return the information that you entered for the admin contact.
--
-- * 'gddrsAutoRenew' - Specifies whether the domain registration is set to renew automatically.
--
-- * 'gddrsAbuseContactPhone' - Phone number for reporting abuse.
--
-- * 'gddrsRegistrarURL' - Web address of the registrar.
--
-- * 'gddrsAbuseContactEmail' - Email address to contact to report incorrect contact information for a domain, to report that the domain is being used to send spam, to report that someone is cybersquatting on a domain name, or report some other type of abuse.
--
-- * 'gddrsExpirationDate' - The date when the registration for the domain is set to expire. The date and time is in Coordinated Universal time (UTC).
--
-- * 'gddrsCreationDate' - The date when the domain was created as found in the response to a WHOIS query. The date and time is in Coordinated Universal time (UTC).
--
-- * 'gddrsRegistrarName' - Name of the registrar of the domain as identified in the registry. Domains with a .com, .net, or .org TLD are registered by Amazon Registrar. All other domains are registered by our registrar associate, Gandi. The value for domains that are registered by Gandi is @"GANDI SAS"@ .
--
-- * 'gddrsReseller' - Reseller of the domain. Domains registered or transferred using Amazon Route 53 domains will have @"Amazon"@ as the reseller.
--
-- * 'gddrsStatusList' - An array of domain name status codes, also known as Extensible Provisioning Protocol (EPP) status codes. ICANN, the organization that maintains a central database of domain names, has developed a set of domain name status codes that tell you the status of a variety of operations on a domain name, for example, registering a domain name, transferring a domain name to another registrar, renewing the registration for a domain name, and so on. All registrars use this same set of status codes. For a current list of domain name status codes and an explanation of what each code means, go to the <https://www.icann.org/ ICANN website> and search for @epp status codes@ . (Search on the ICANN website; web searches sometimes return an old version of the document.)
--
-- * 'gddrsResponseStatus' - -- | The response status code.
--
-- * 'gddrsDomainName' - The name of a domain.
--
-- * 'gddrsNameservers' - The name of the domain.
--
-- * 'gddrsAdminContact' - Provides details about the domain administrative contact.
--
-- * 'gddrsRegistrantContact' - Provides details about the domain registrant.
--
-- * 'gddrsTechContact' - Provides details about the domain technical contact.
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


-- | Specifies whether contact information is concealed from WHOIS queries. If the value is @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If the value is @false@ , WHOIS queries return the information that you entered for the technical contact.
gddrsTechPrivacy :: Lens' GetDomainDetailResponse (Maybe Bool)
gddrsTechPrivacy = lens _gddrsTechPrivacy (\ s a -> s{_gddrsTechPrivacy = a})

-- | Reserved for future use.
gddrsDNSSec :: Lens' GetDomainDetailResponse (Maybe Text)
gddrsDNSSec = lens _gddrsDNSSec (\ s a -> s{_gddrsDNSSec = a})

-- | The fully qualified name of the WHOIS server that can answer the WHOIS query for the domain.
gddrsWhoIsServer :: Lens' GetDomainDetailResponse (Maybe Text)
gddrsWhoIsServer = lens _gddrsWhoIsServer (\ s a -> s{_gddrsWhoIsServer = a})

-- | Reserved for future use.
gddrsRegistryDomainId :: Lens' GetDomainDetailResponse (Maybe Text)
gddrsRegistryDomainId = lens _gddrsRegistryDomainId (\ s a -> s{_gddrsRegistryDomainId = a})

-- | Specifies whether contact information is concealed from WHOIS queries. If the value is @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If the value is @false@ , WHOIS queries return the information that you entered for the registrant contact (domain owner).
gddrsRegistrantPrivacy :: Lens' GetDomainDetailResponse (Maybe Bool)
gddrsRegistrantPrivacy = lens _gddrsRegistrantPrivacy (\ s a -> s{_gddrsRegistrantPrivacy = a})

-- | The last updated date of the domain as found in the response to a WHOIS query. The date and time is in Coordinated Universal time (UTC).
gddrsUpdatedDate :: Lens' GetDomainDetailResponse (Maybe UTCTime)
gddrsUpdatedDate = lens _gddrsUpdatedDate (\ s a -> s{_gddrsUpdatedDate = a}) . mapping _Time

-- | Specifies whether contact information is concealed from WHOIS queries. If the value is @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If the value is @false@ , WHOIS queries return the information that you entered for the admin contact.
gddrsAdminPrivacy :: Lens' GetDomainDetailResponse (Maybe Bool)
gddrsAdminPrivacy = lens _gddrsAdminPrivacy (\ s a -> s{_gddrsAdminPrivacy = a})

-- | Specifies whether the domain registration is set to renew automatically.
gddrsAutoRenew :: Lens' GetDomainDetailResponse (Maybe Bool)
gddrsAutoRenew = lens _gddrsAutoRenew (\ s a -> s{_gddrsAutoRenew = a})

-- | Phone number for reporting abuse.
gddrsAbuseContactPhone :: Lens' GetDomainDetailResponse (Maybe Text)
gddrsAbuseContactPhone = lens _gddrsAbuseContactPhone (\ s a -> s{_gddrsAbuseContactPhone = a})

-- | Web address of the registrar.
gddrsRegistrarURL :: Lens' GetDomainDetailResponse (Maybe Text)
gddrsRegistrarURL = lens _gddrsRegistrarURL (\ s a -> s{_gddrsRegistrarURL = a})

-- | Email address to contact to report incorrect contact information for a domain, to report that the domain is being used to send spam, to report that someone is cybersquatting on a domain name, or report some other type of abuse.
gddrsAbuseContactEmail :: Lens' GetDomainDetailResponse (Maybe Text)
gddrsAbuseContactEmail = lens _gddrsAbuseContactEmail (\ s a -> s{_gddrsAbuseContactEmail = a})

-- | The date when the registration for the domain is set to expire. The date and time is in Coordinated Universal time (UTC).
gddrsExpirationDate :: Lens' GetDomainDetailResponse (Maybe UTCTime)
gddrsExpirationDate = lens _gddrsExpirationDate (\ s a -> s{_gddrsExpirationDate = a}) . mapping _Time

-- | The date when the domain was created as found in the response to a WHOIS query. The date and time is in Coordinated Universal time (UTC).
gddrsCreationDate :: Lens' GetDomainDetailResponse (Maybe UTCTime)
gddrsCreationDate = lens _gddrsCreationDate (\ s a -> s{_gddrsCreationDate = a}) . mapping _Time

-- | Name of the registrar of the domain as identified in the registry. Domains with a .com, .net, or .org TLD are registered by Amazon Registrar. All other domains are registered by our registrar associate, Gandi. The value for domains that are registered by Gandi is @"GANDI SAS"@ .
gddrsRegistrarName :: Lens' GetDomainDetailResponse (Maybe Text)
gddrsRegistrarName = lens _gddrsRegistrarName (\ s a -> s{_gddrsRegistrarName = a})

-- | Reseller of the domain. Domains registered or transferred using Amazon Route 53 domains will have @"Amazon"@ as the reseller.
gddrsReseller :: Lens' GetDomainDetailResponse (Maybe Text)
gddrsReseller = lens _gddrsReseller (\ s a -> s{_gddrsReseller = a})

-- | An array of domain name status codes, also known as Extensible Provisioning Protocol (EPP) status codes. ICANN, the organization that maintains a central database of domain names, has developed a set of domain name status codes that tell you the status of a variety of operations on a domain name, for example, registering a domain name, transferring a domain name to another registrar, renewing the registration for a domain name, and so on. All registrars use this same set of status codes. For a current list of domain name status codes and an explanation of what each code means, go to the <https://www.icann.org/ ICANN website> and search for @epp status codes@ . (Search on the ICANN website; web searches sometimes return an old version of the document.)
gddrsStatusList :: Lens' GetDomainDetailResponse [Text]
gddrsStatusList = lens _gddrsStatusList (\ s a -> s{_gddrsStatusList = a}) . _Default . _Coerce

-- | -- | The response status code.
gddrsResponseStatus :: Lens' GetDomainDetailResponse Int
gddrsResponseStatus = lens _gddrsResponseStatus (\ s a -> s{_gddrsResponseStatus = a})

-- | The name of a domain.
gddrsDomainName :: Lens' GetDomainDetailResponse Text
gddrsDomainName = lens _gddrsDomainName (\ s a -> s{_gddrsDomainName = a})

-- | The name of the domain.
gddrsNameservers :: Lens' GetDomainDetailResponse [Nameserver]
gddrsNameservers = lens _gddrsNameservers (\ s a -> s{_gddrsNameservers = a}) . _Coerce

-- | Provides details about the domain administrative contact.
gddrsAdminContact :: Lens' GetDomainDetailResponse ContactDetail
gddrsAdminContact = lens _gddrsAdminContact (\ s a -> s{_gddrsAdminContact = a}) . _Sensitive

-- | Provides details about the domain registrant.
gddrsRegistrantContact :: Lens' GetDomainDetailResponse ContactDetail
gddrsRegistrantContact = lens _gddrsRegistrantContact (\ s a -> s{_gddrsRegistrantContact = a}) . _Sensitive

-- | Provides details about the domain technical contact.
gddrsTechContact :: Lens' GetDomainDetailResponse ContactDetail
gddrsTechContact = lens _gddrsTechContact (\ s a -> s{_gddrsTechContact = a}) . _Sensitive

instance NFData GetDomainDetailResponse where
