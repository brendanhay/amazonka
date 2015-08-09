{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.UpdateDomainContactPrivacy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation updates the specified domain contact\'s privacy setting.
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
-- /See:/ <http://docs.aws.amazon.com/Route53/latest/APIReference/api-UpdateDomainContactPrivacy.html AWS API Reference> for UpdateDomainContactPrivacy.
module Network.AWS.Route53Domains.UpdateDomainContactPrivacy
    (
    -- * Creating a Request
      UpdateDomainContactPrivacy
    , updateDomainContactPrivacy
    -- * Request Lenses
    , udcpTechPrivacy
    , udcpRegistrantPrivacy
    , udcpAdminPrivacy
    , udcpDomainName

    -- * Destructuring the Response
    , UpdateDomainContactPrivacyResponse
    , updateDomainContactPrivacyResponse
    -- * Response Lenses
    , udcprsStatus
    , udcprsOperationId
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53Domains.Types
import           Network.AWS.Route53Domains.Types.Product

-- | The UpdateDomainContactPrivacy request includes the following elements.
--
-- /See:/ 'updateDomainContactPrivacy' smart constructor.
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
data UpdateDomainContactPrivacy = UpdateDomainContactPrivacy'
    { _udcpTechPrivacy       :: !(Maybe Bool)
    , _udcpRegistrantPrivacy :: !(Maybe Bool)
    , _udcpAdminPrivacy      :: !(Maybe Bool)
    , _udcpDomainName        :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateDomainContactPrivacy' smart constructor.
updateDomainContactPrivacy :: Text -> UpdateDomainContactPrivacy
updateDomainContactPrivacy pDomainName_ =
    UpdateDomainContactPrivacy'
    { _udcpTechPrivacy = Nothing
    , _udcpRegistrantPrivacy = Nothing
    , _udcpAdminPrivacy = Nothing
    , _udcpDomainName = pDomainName_
    }

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
                   (pure (fromEnum s)) <*> (x .:> "OperationId"))

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

-- | The UpdateDomainContactPrivacy response includes the following element.
--
-- /See:/ 'updateDomainContactPrivacyResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'udcprsStatus'
--
-- * 'udcprsOperationId'
data UpdateDomainContactPrivacyResponse = UpdateDomainContactPrivacyResponse'
    { _udcprsStatus      :: !Int
    , _udcprsOperationId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateDomainContactPrivacyResponse' smart constructor.
updateDomainContactPrivacyResponse :: Int -> Text -> UpdateDomainContactPrivacyResponse
updateDomainContactPrivacyResponse pStatus_ pOperationId_ =
    UpdateDomainContactPrivacyResponse'
    { _udcprsStatus = pStatus_
    , _udcprsOperationId = pOperationId_
    }

-- | Undocumented member.
udcprsStatus :: Lens' UpdateDomainContactPrivacyResponse Int
udcprsStatus = lens _udcprsStatus (\ s a -> s{_udcprsStatus = a});

-- | Identifier for tracking the progress of the request. To use this ID to
-- query the operation status, use GetOperationDetail.
--
-- Type: String
--
-- Default: None
--
-- Constraints: Maximum 255 characters.
udcprsOperationId :: Lens' UpdateDomainContactPrivacyResponse Text
udcprsOperationId = lens _udcprsOperationId (\ s a -> s{_udcprsOperationId = a});
