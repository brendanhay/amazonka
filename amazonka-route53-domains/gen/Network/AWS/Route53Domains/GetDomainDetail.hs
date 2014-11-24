{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53Domains.GetDomainDetail
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation returns detailed information about the domain. The domain's
-- contact information is also returned as part of the output.
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
    , gddrAbuseContactEmail
    , gddrAbuseContactPhone
    , gddrAdminContact
    , gddrAdminPrivacy
    , gddrAutoRenew
    , gddrCreationDate
    , gddrDnsSec
    , gddrDomainName
    , gddrExpirationDate
    , gddrNameservers
    , gddrRegistrantContact
    , gddrRegistrantPrivacy
    , gddrRegistrarName
    , gddrRegistrarUrl
    , gddrRegistryDomainId
    , gddrReseller
    , gddrStatusList
    , gddrTechContact
    , gddrTechPrivacy
    , gddrUpdatedDate
    , gddrWhoIsServer
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.Route53Domains.Types
import qualified GHC.Exts

newtype GetDomainDetail = GetDomainDetail
    { _gddDomainName :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'GetDomainDetail' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gddDomainName' @::@ 'Text'
--
getDomainDetail :: Text -- ^ 'gddDomainName'
                -> GetDomainDetail
getDomainDetail p1 = GetDomainDetail
    { _gddDomainName = p1
    }

-- | The name of a domain. Type: String Default: None Constraints: The domain
-- name can contain only the letters a through z, the numbers 0 through 9,
-- and hyphen (-). Internationalized Domain Names are not supported.
-- Required: Yes.
gddDomainName :: Lens' GetDomainDetail Text
gddDomainName = lens _gddDomainName (\s a -> s { _gddDomainName = a })

data GetDomainDetailResponse = GetDomainDetailResponse
    { _gddrAbuseContactEmail :: Maybe Text
    , _gddrAbuseContactPhone :: Maybe Text
    , _gddrAdminContact      :: ContactDetail
    , _gddrAdminPrivacy      :: Maybe Bool
    , _gddrAutoRenew         :: Maybe Bool
    , _gddrCreationDate      :: Maybe RFC822
    , _gddrDnsSec            :: Maybe Text
    , _gddrDomainName        :: Text
    , _gddrExpirationDate    :: Maybe RFC822
    , _gddrNameservers       :: List "Nameservers" Nameserver
    , _gddrRegistrantContact :: ContactDetail
    , _gddrRegistrantPrivacy :: Maybe Bool
    , _gddrRegistrarName     :: Maybe Text
    , _gddrRegistrarUrl      :: Maybe Text
    , _gddrRegistryDomainId  :: Maybe Text
    , _gddrReseller          :: Maybe Text
    , _gddrStatusList        :: List "StatusList" Text
    , _gddrTechContact       :: ContactDetail
    , _gddrTechPrivacy       :: Maybe Bool
    , _gddrUpdatedDate       :: Maybe RFC822
    , _gddrWhoIsServer       :: Maybe Text
    } deriving (Eq, Show)

-- | 'GetDomainDetailResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gddrAbuseContactEmail' @::@ 'Maybe' 'Text'
--
-- * 'gddrAbuseContactPhone' @::@ 'Maybe' 'Text'
--
-- * 'gddrAdminContact' @::@ 'ContactDetail'
--
-- * 'gddrAdminPrivacy' @::@ 'Maybe' 'Bool'
--
-- * 'gddrAutoRenew' @::@ 'Maybe' 'Bool'
--
-- * 'gddrCreationDate' @::@ 'Maybe' 'UTCTime'
--
-- * 'gddrDnsSec' @::@ 'Maybe' 'Text'
--
-- * 'gddrDomainName' @::@ 'Text'
--
-- * 'gddrExpirationDate' @::@ 'Maybe' 'UTCTime'
--
-- * 'gddrNameservers' @::@ ['Nameserver']
--
-- * 'gddrRegistrantContact' @::@ 'ContactDetail'
--
-- * 'gddrRegistrantPrivacy' @::@ 'Maybe' 'Bool'
--
-- * 'gddrRegistrarName' @::@ 'Maybe' 'Text'
--
-- * 'gddrRegistrarUrl' @::@ 'Maybe' 'Text'
--
-- * 'gddrRegistryDomainId' @::@ 'Maybe' 'Text'
--
-- * 'gddrReseller' @::@ 'Maybe' 'Text'
--
-- * 'gddrStatusList' @::@ ['Text']
--
-- * 'gddrTechContact' @::@ 'ContactDetail'
--
-- * 'gddrTechPrivacy' @::@ 'Maybe' 'Bool'
--
-- * 'gddrUpdatedDate' @::@ 'Maybe' 'UTCTime'
--
-- * 'gddrWhoIsServer' @::@ 'Maybe' 'Text'
--
getDomainDetailResponse :: Text -- ^ 'gddrDomainName'
                        -> ContactDetail -- ^ 'gddrAdminContact'
                        -> ContactDetail -- ^ 'gddrRegistrantContact'
                        -> ContactDetail -- ^ 'gddrTechContact'
                        -> GetDomainDetailResponse
getDomainDetailResponse p1 p2 p3 p4 = GetDomainDetailResponse
    { _gddrDomainName        = p1
    , _gddrAdminContact      = p2
    , _gddrRegistrantContact = p3
    , _gddrTechContact       = p4
    , _gddrNameservers       = mempty
    , _gddrAutoRenew         = Nothing
    , _gddrAdminPrivacy      = Nothing
    , _gddrRegistrantPrivacy = Nothing
    , _gddrTechPrivacy       = Nothing
    , _gddrRegistrarName     = Nothing
    , _gddrWhoIsServer       = Nothing
    , _gddrRegistrarUrl      = Nothing
    , _gddrAbuseContactEmail = Nothing
    , _gddrAbuseContactPhone = Nothing
    , _gddrRegistryDomainId  = Nothing
    , _gddrCreationDate      = Nothing
    , _gddrUpdatedDate       = Nothing
    , _gddrExpirationDate    = Nothing
    , _gddrReseller          = Nothing
    , _gddrDnsSec            = Nothing
    , _gddrStatusList        = mempty
    }

-- | Email address to contact to report incorrect contact information for a
-- domain, to report that the domain is being used to send spam, to report
-- that someone is cybersquatting on a domain name, or report some other
-- type of abuse. Type: String.
gddrAbuseContactEmail :: Lens' GetDomainDetailResponse (Maybe Text)
gddrAbuseContactEmail =
    lens _gddrAbuseContactEmail (\s a -> s { _gddrAbuseContactEmail = a })

-- | Phone number for reporting abuse. Type: String.
gddrAbuseContactPhone :: Lens' GetDomainDetailResponse (Maybe Text)
gddrAbuseContactPhone =
    lens _gddrAbuseContactPhone (\s a -> s { _gddrAbuseContactPhone = a })

-- | Provides details about the domain administrative contact. Type: Complex
-- Children: 'FirstName', 'MiddleName', 'LastName', 'ContactType',
-- 'OrganizationName', 'AddressLine1', 'AddressLine2', 'City', 'State',
-- 'CountryCode', 'ZipCode', 'PhoneNumber', 'Email', 'Fax', 'ExtraParams'.
gddrAdminContact :: Lens' GetDomainDetailResponse ContactDetail
gddrAdminContact = lens _gddrAdminContact (\s a -> s { _gddrAdminContact = a })

-- | Specifies whether contact information for the admin contact is concealed
-- from WHOIS queries. If the value is 'true', WHOIS ("who is") queries will
-- return contact information for our registrar partner, Gandi, instead of
-- the contact information that you enter. Type: Boolean.
gddrAdminPrivacy :: Lens' GetDomainDetailResponse (Maybe Bool)
gddrAdminPrivacy = lens _gddrAdminPrivacy (\s a -> s { _gddrAdminPrivacy = a })

-- | Specifies whether the domain registration is set to renew automatically.
-- Type: Boolean.
gddrAutoRenew :: Lens' GetDomainDetailResponse (Maybe Bool)
gddrAutoRenew = lens _gddrAutoRenew (\s a -> s { _gddrAutoRenew = a })

-- | The date when the domain was created as found in the response to a WHOIS
-- query. The date format is Unix time.
gddrCreationDate :: Lens' GetDomainDetailResponse (Maybe UTCTime)
gddrCreationDate = lens _gddrCreationDate (\s a -> s { _gddrCreationDate = a }) . mapping _Time

-- | Reserved for future use.
gddrDnsSec :: Lens' GetDomainDetailResponse (Maybe Text)
gddrDnsSec = lens _gddrDnsSec (\s a -> s { _gddrDnsSec = a })

-- | The name of a domain. Type: String.
gddrDomainName :: Lens' GetDomainDetailResponse Text
gddrDomainName = lens _gddrDomainName (\s a -> s { _gddrDomainName = a })

-- | The date when the registration for the domain is set to expire. The date
-- format is Unix time.
gddrExpirationDate :: Lens' GetDomainDetailResponse (Maybe UTCTime)
gddrExpirationDate =
    lens _gddrExpirationDate (\s a -> s { _gddrExpirationDate = a })
        . mapping _Time

-- | The name of the domain. Type: String.
gddrNameservers :: Lens' GetDomainDetailResponse [Nameserver]
gddrNameservers = lens _gddrNameservers (\s a -> s { _gddrNameservers = a }) . _List

-- | Provides details about the domain registrant. Type: Complex Children:
-- 'FirstName', 'MiddleName', 'LastName', 'ContactType', 'OrganizationName',
-- 'AddressLine1', 'AddressLine2', 'City', 'State', 'CountryCode',
-- 'ZipCode', 'PhoneNumber', 'Email', 'Fax', 'ExtraParams'.
gddrRegistrantContact :: Lens' GetDomainDetailResponse ContactDetail
gddrRegistrantContact =
    lens _gddrRegistrantContact (\s a -> s { _gddrRegistrantContact = a })

-- | Specifies whether contact information for the registrant contact is
-- concealed from WHOIS queries. If the value is 'true', WHOIS ("who is")
-- queries will return contact information for our registrar partner, Gandi,
-- instead of the contact information that you enter. Type: Boolean.
gddrRegistrantPrivacy :: Lens' GetDomainDetailResponse (Maybe Bool)
gddrRegistrantPrivacy =
    lens _gddrRegistrantPrivacy (\s a -> s { _gddrRegistrantPrivacy = a })

-- | Name of the registrar of the domain as identified in the registry. Amazon
-- Route 53 domains are registered by registrar Gandi. The value is '"GANDI
-- SAS"'. Type: String.
gddrRegistrarName :: Lens' GetDomainDetailResponse (Maybe Text)
gddrRegistrarName =
    lens _gddrRegistrarName (\s a -> s { _gddrRegistrarName = a })

-- | Web address of the registrar. Type: String.
gddrRegistrarUrl :: Lens' GetDomainDetailResponse (Maybe Text)
gddrRegistrarUrl = lens _gddrRegistrarUrl (\s a -> s { _gddrRegistrarUrl = a })

-- | Reserved for future use.
gddrRegistryDomainId :: Lens' GetDomainDetailResponse (Maybe Text)
gddrRegistryDomainId =
    lens _gddrRegistryDomainId (\s a -> s { _gddrRegistryDomainId = a })

-- | Reseller of the domain. Domains registered or transferred using Amazon
-- Route 53 domains will have '"Amazon"' as the reseller. Type: String.
gddrReseller :: Lens' GetDomainDetailResponse (Maybe Text)
gddrReseller = lens _gddrReseller (\s a -> s { _gddrReseller = a })

-- | An array of domain name status codes, also known as Extensible
-- Provisioning Protocol (EPP) status codes. ICANN, the organization that
-- maintains a central database of domain names, has developed a set of
-- domain name status codes that tell you the status of a variety of
-- operations on a domain name, for example, registering a domain name,
-- transferring a domain name to another registrar, renewing the
-- registration for a domain name, and so on. All registrars use this same
-- set of status codes. For a current list of domain name status codes and
-- an explanation of what each code means, go to the <https://www.icann.org/
-- ICANN website> and search for 'epp status codes'. (Search on the ICANN
-- website; web searches sometimes return an old version of the document.)
-- Type: Array of String.
gddrStatusList :: Lens' GetDomainDetailResponse [Text]
gddrStatusList = lens _gddrStatusList (\s a -> s { _gddrStatusList = a }) . _List

-- | Provides details about the domain technical contact. Type: Complex
-- Children: 'FirstName', 'MiddleName', 'LastName', 'ContactType',
-- 'OrganizationName', 'AddressLine1', 'AddressLine2', 'City', 'State',
-- 'CountryCode', 'ZipCode', 'PhoneNumber', 'Email', 'Fax', 'ExtraParams'.
gddrTechContact :: Lens' GetDomainDetailResponse ContactDetail
gddrTechContact = lens _gddrTechContact (\s a -> s { _gddrTechContact = a })

-- | Specifies whether contact information for the tech contact is concealed
-- from WHOIS queries. If the value is 'true', WHOIS ("who is") queries will
-- return contact information for our registrar partner, Gandi, instead of
-- the contact information that you enter. Type: Boolean.
gddrTechPrivacy :: Lens' GetDomainDetailResponse (Maybe Bool)
gddrTechPrivacy = lens _gddrTechPrivacy (\s a -> s { _gddrTechPrivacy = a })

-- | The last updated date of the domain as found in the response to a WHOIS
-- query. The date format is Unix time.
gddrUpdatedDate :: Lens' GetDomainDetailResponse (Maybe UTCTime)
gddrUpdatedDate = lens _gddrUpdatedDate (\s a -> s { _gddrUpdatedDate = a }) . mapping _Time

-- | The fully qualified name of the WHOIS server that can answer the WHOIS
-- query for the domain. Type: String.
gddrWhoIsServer :: Lens' GetDomainDetailResponse (Maybe Text)
gddrWhoIsServer = lens _gddrWhoIsServer (\s a -> s { _gddrWhoIsServer = a })

instance ToPath GetDomainDetail where
    toPath = const "/"

instance ToQuery GetDomainDetail where
    toQuery = const mempty

instance ToHeaders GetDomainDetail

instance ToJSON GetDomainDetail where
    toJSON GetDomainDetail{..} = object
        [ "DomainName" .= _gddDomainName
        ]

instance AWSRequest GetDomainDetail where
    type Sv GetDomainDetail = Route53Domains
    type Rs GetDomainDetail = GetDomainDetailResponse

    request  = post "GetDomainDetail"
    response = jsonResponse

instance FromJSON GetDomainDetailResponse where
    parseJSON = withObject "GetDomainDetailResponse" $ \o -> GetDomainDetailResponse
        <$> o .:? "AbuseContactEmail"
        <*> o .:? "AbuseContactPhone"
        <*> o .:  "AdminContact"
        <*> o .:? "AdminPrivacy"
        <*> o .:? "AutoRenew"
        <*> o .:? "CreationDate"
        <*> o .:? "DnsSec"
        <*> o .:  "DomainName"
        <*> o .:? "ExpirationDate"
        <*> o .:  "Nameservers"
        <*> o .:  "RegistrantContact"
        <*> o .:? "RegistrantPrivacy"
        <*> o .:? "RegistrarName"
        <*> o .:? "RegistrarUrl"
        <*> o .:? "RegistryDomainId"
        <*> o .:? "Reseller"
        <*> o .:  "StatusList"
        <*> o .:  "TechContact"
        <*> o .:? "TechPrivacy"
        <*> o .:? "UpdatedDate"
        <*> o .:? "WhoIsServer"
