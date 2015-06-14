{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Route53Domains.UpdateDomainContactPrivacy
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

-- | This operation updates the specified domain contact\'s privacy setting.
-- When the privacy option is enabled, personal information such as postal
-- or email address is hidden from the results of a public WHOIS query. The
-- privacy services are provided by the AWS registrar, Gandi. For more
-- information, see the
-- <http://www.gandi.net/domain/whois/?currency=USD&amp;lang=en Gandi privacy features>.
--
-- This operation only affects the privacy of the specified contact type
-- (registrant, administrator, or tech). Successful acceptance returns an
-- operation ID that you can use with GetOperationDetail to track the
-- progress and completion of the action. If the request is not completed
-- successfully, the domain registrant will be notified by email.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/api-UpdateDomainContactPrivacy.html>
module Network.AWS.Route53Domains.UpdateDomainContactPrivacy
    (
    -- * Request
      UpdateDomainContactPrivacy
    -- ** Request constructor
    , updateDomainContactPrivacy
    -- ** Request lenses
    , udcpTechPrivacy
    , udcpRegistrantPrivacy
    , udcpAdminPrivacy
    , udcpDomainName

    -- * Response
    , UpdateDomainContactPrivacyResponse
    -- ** Response constructor
    , updateDomainContactPrivacyResponse
    -- ** Response lenses
    , udcprOperationId
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.Route53Domains.Types

-- | /See:/ 'updateDomainContactPrivacy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'udcpTechPrivacy'
--
-- * 'udcpRegistrantPrivacy'
--
-- * 'udcpAdminPrivacy'
--
-- * 'udcpDomainName'
data UpdateDomainContactPrivacy = UpdateDomainContactPrivacy'{_udcpTechPrivacy :: Maybe Bool, _udcpRegistrantPrivacy :: Maybe Bool, _udcpAdminPrivacy :: Maybe Bool, _udcpDomainName :: Text} deriving (Eq, Read, Show)

-- | 'UpdateDomainContactPrivacy' smart constructor.
updateDomainContactPrivacy :: Text -> UpdateDomainContactPrivacy
updateDomainContactPrivacy pDomainName = UpdateDomainContactPrivacy'{_udcpTechPrivacy = Nothing, _udcpRegistrantPrivacy = Nothing, _udcpAdminPrivacy = Nothing, _udcpDomainName = pDomainName};

-- | Whether you want to conceal contact information from WHOIS queries. If
-- you specify true, WHOIS (\"who is\") queries will return contact
-- information for our registrar partner, Gandi, instead of the contact
-- information that you enter.
--
-- Type: Boolean
--
-- Default: None
--
-- Valid values: @true@ | @false@
--
-- Required: No
udcpTechPrivacy :: Lens' UpdateDomainContactPrivacy (Maybe Bool)
udcpTechPrivacy = lens _udcpTechPrivacy (\ s a -> s{_udcpTechPrivacy = a});

-- | Whether you want to conceal contact information from WHOIS queries. If
-- you specify true, WHOIS (\"who is\") queries will return contact
-- information for our registrar partner, Gandi, instead of the contact
-- information that you enter.
--
-- Type: Boolean
--
-- Default: None
--
-- Valid values: @true@ | @false@
--
-- Required: No
udcpRegistrantPrivacy :: Lens' UpdateDomainContactPrivacy (Maybe Bool)
udcpRegistrantPrivacy = lens _udcpRegistrantPrivacy (\ s a -> s{_udcpRegistrantPrivacy = a});

-- | Whether you want to conceal contact information from WHOIS queries. If
-- you specify true, WHOIS (\"who is\") queries will return contact
-- information for our registrar partner, Gandi, instead of the contact
-- information that you enter.
--
-- Type: Boolean
--
-- Default: None
--
-- Valid values: @true@ | @false@
--
-- Required: No
udcpAdminPrivacy :: Lens' UpdateDomainContactPrivacy (Maybe Bool)
udcpAdminPrivacy = lens _udcpAdminPrivacy (\ s a -> s{_udcpAdminPrivacy = a});

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
udcpDomainName :: Lens' UpdateDomainContactPrivacy Text
udcpDomainName = lens _udcpDomainName (\ s a -> s{_udcpDomainName = a});

instance AWSRequest UpdateDomainContactPrivacy where
        type Sv UpdateDomainContactPrivacy = Route53Domains
        type Rs UpdateDomainContactPrivacy =
             UpdateDomainContactPrivacyResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 UpdateDomainContactPrivacyResponse' <$>
                   x .:> "OperationId")

instance ToHeaders UpdateDomainContactPrivacy where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Route53Domains_v20140515.UpdateDomainContactPrivacy"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateDomainContactPrivacy where
        toJSON UpdateDomainContactPrivacy'{..}
          = object
              ["TechPrivacy" .= _udcpTechPrivacy,
               "RegistrantPrivacy" .= _udcpRegistrantPrivacy,
               "AdminPrivacy" .= _udcpAdminPrivacy,
               "DomainName" .= _udcpDomainName]

instance ToPath UpdateDomainContactPrivacy where
        toPath = const "/"

instance ToQuery UpdateDomainContactPrivacy where
        toQuery = const mempty

-- | /See:/ 'updateDomainContactPrivacyResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'udcprOperationId'
newtype UpdateDomainContactPrivacyResponse = UpdateDomainContactPrivacyResponse'{_udcprOperationId :: Text} deriving (Eq, Read, Show)

-- | 'UpdateDomainContactPrivacyResponse' smart constructor.
updateDomainContactPrivacyResponse :: Text -> UpdateDomainContactPrivacyResponse
updateDomainContactPrivacyResponse pOperationId = UpdateDomainContactPrivacyResponse'{_udcprOperationId = pOperationId};

-- | Identifier for tracking the progress of the request. To use this ID to
-- query the operation status, use GetOperationDetail.
--
-- Type: String
--
-- Default: None
--
-- Constraints: Maximum 255 characters.
udcprOperationId :: Lens' UpdateDomainContactPrivacyResponse Text
udcprOperationId = lens _udcprOperationId (\ s a -> s{_udcprOperationId = a});
