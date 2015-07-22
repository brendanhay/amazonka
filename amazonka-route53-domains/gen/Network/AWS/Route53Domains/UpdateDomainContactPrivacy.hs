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
-- Stability   : experimental
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
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/api-UpdateDomainContactPrivacy.html>
module Network.AWS.Route53Domains.UpdateDomainContactPrivacy
    (
    -- * Request
      UpdateDomainContactPrivacy
    -- ** Request constructor
    , updateDomainContactPrivacy
    -- ** Request lenses
    , udcprqTechPrivacy
    , udcprqRegistrantPrivacy
    , udcprqAdminPrivacy
    , udcprqDomainName

    -- * Response
    , UpdateDomainContactPrivacyResponse
    -- ** Response constructor
    , updateDomainContactPrivacyResponse
    -- ** Response lenses
    , udcprsStatus
    , udcprsOperationId
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53Domains.Types

-- | The UpdateDomainContactPrivacy request includes the following elements.
--
-- /See:/ 'updateDomainContactPrivacy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'udcprqTechPrivacy'
--
-- * 'udcprqRegistrantPrivacy'
--
-- * 'udcprqAdminPrivacy'
--
-- * 'udcprqDomainName'
data UpdateDomainContactPrivacy = UpdateDomainContactPrivacy'
    { _udcprqTechPrivacy       :: !(Maybe Bool)
    , _udcprqRegistrantPrivacy :: !(Maybe Bool)
    , _udcprqAdminPrivacy      :: !(Maybe Bool)
    , _udcprqDomainName        :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateDomainContactPrivacy' smart constructor.
updateDomainContactPrivacy :: Text -> UpdateDomainContactPrivacy
updateDomainContactPrivacy pDomainName_ =
    UpdateDomainContactPrivacy'
    { _udcprqTechPrivacy = Nothing
    , _udcprqRegistrantPrivacy = Nothing
    , _udcprqAdminPrivacy = Nothing
    , _udcprqDomainName = pDomainName_
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
udcprqTechPrivacy :: Lens' UpdateDomainContactPrivacy (Maybe Bool)
udcprqTechPrivacy = lens _udcprqTechPrivacy (\ s a -> s{_udcprqTechPrivacy = a});

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
udcprqRegistrantPrivacy :: Lens' UpdateDomainContactPrivacy (Maybe Bool)
udcprqRegistrantPrivacy = lens _udcprqRegistrantPrivacy (\ s a -> s{_udcprqRegistrantPrivacy = a});

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
udcprqAdminPrivacy :: Lens' UpdateDomainContactPrivacy (Maybe Bool)
udcprqAdminPrivacy = lens _udcprqAdminPrivacy (\ s a -> s{_udcprqAdminPrivacy = a});

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
udcprqDomainName :: Lens' UpdateDomainContactPrivacy Text
udcprqDomainName = lens _udcprqDomainName (\ s a -> s{_udcprqDomainName = a});

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
              ["TechPrivacy" .= _udcprqTechPrivacy,
               "RegistrantPrivacy" .= _udcprqRegistrantPrivacy,
               "AdminPrivacy" .= _udcprqAdminPrivacy,
               "DomainName" .= _udcprqDomainName]

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

-- | FIXME: Undocumented member.
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
