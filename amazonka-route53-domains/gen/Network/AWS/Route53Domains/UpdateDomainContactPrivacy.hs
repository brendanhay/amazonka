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
-- Module      : Network.AWS.Route53Domains.UpdateDomainContactPrivacy
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation updates the specified domain contact's privacy setting. When the privacy option is enabled, personal information such as postal or email address is hidden from the results of a public WHOIS query. The privacy services are provided by the AWS registrar, Gandi. For more information, see the <http://www.gandi.net/domain/whois/?currency=USD&amp;amp;lang=en Gandi privacy features> .
--
--
-- This operation only affects the privacy of the specified contact type (registrant, administrator, or tech). Successful acceptance returns an operation ID that you can use with 'GetOperationDetail' to track the progress and completion of the action. If the request is not completed successfully, the domain registrant will be notified by email.
--
module Network.AWS.Route53Domains.UpdateDomainContactPrivacy
    (
    -- * Creating a Request
      updateDomainContactPrivacy
    , UpdateDomainContactPrivacy
    -- * Request Lenses
    , udcpTechPrivacy
    , udcpRegistrantPrivacy
    , udcpAdminPrivacy
    , udcpDomainName

    -- * Destructuring the Response
    , updateDomainContactPrivacyResponse
    , UpdateDomainContactPrivacyResponse
    -- * Response Lenses
    , udcprsResponseStatus
    , udcprsOperationId
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53Domains.Types
import           Network.AWS.Route53Domains.Types.Product

-- | The UpdateDomainContactPrivacy request includes the following elements.
--
--
--
-- /See:/ 'updateDomainContactPrivacy' smart constructor.
data UpdateDomainContactPrivacy = UpdateDomainContactPrivacy'
    { _udcpTechPrivacy       :: !(Maybe Bool)
    , _udcpRegistrantPrivacy :: !(Maybe Bool)
    , _udcpAdminPrivacy      :: !(Maybe Bool)
    , _udcpDomainName        :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateDomainContactPrivacy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udcpTechPrivacy' - Whether you want to conceal contact information from WHOIS queries. If you specify @true@ , WHOIS ("who is") queries will return contact information for our registrar partner, Gandi, instead of the contact information that you enter.
--
-- * 'udcpRegistrantPrivacy' - Whether you want to conceal contact information from WHOIS queries. If you specify @true@ , WHOIS ("who is") queries will return contact information for our registrar partner, Gandi, instead of the contact information that you enter.
--
-- * 'udcpAdminPrivacy' - Whether you want to conceal contact information from WHOIS queries. If you specify @true@ , WHOIS ("who is") queries will return contact information for our registrar partner, Gandi, instead of the contact information that you enter.
--
-- * 'udcpDomainName' - The name of the domain that you want to update the privacy setting for.
updateDomainContactPrivacy
    :: Text -- ^ 'udcpDomainName'
    -> UpdateDomainContactPrivacy
updateDomainContactPrivacy pDomainName_ =
    UpdateDomainContactPrivacy'
    { _udcpTechPrivacy = Nothing
    , _udcpRegistrantPrivacy = Nothing
    , _udcpAdminPrivacy = Nothing
    , _udcpDomainName = pDomainName_
    }

-- | Whether you want to conceal contact information from WHOIS queries. If you specify @true@ , WHOIS ("who is") queries will return contact information for our registrar partner, Gandi, instead of the contact information that you enter.
udcpTechPrivacy :: Lens' UpdateDomainContactPrivacy (Maybe Bool)
udcpTechPrivacy = lens _udcpTechPrivacy (\ s a -> s{_udcpTechPrivacy = a});

-- | Whether you want to conceal contact information from WHOIS queries. If you specify @true@ , WHOIS ("who is") queries will return contact information for our registrar partner, Gandi, instead of the contact information that you enter.
udcpRegistrantPrivacy :: Lens' UpdateDomainContactPrivacy (Maybe Bool)
udcpRegistrantPrivacy = lens _udcpRegistrantPrivacy (\ s a -> s{_udcpRegistrantPrivacy = a});

-- | Whether you want to conceal contact information from WHOIS queries. If you specify @true@ , WHOIS ("who is") queries will return contact information for our registrar partner, Gandi, instead of the contact information that you enter.
udcpAdminPrivacy :: Lens' UpdateDomainContactPrivacy (Maybe Bool)
udcpAdminPrivacy = lens _udcpAdminPrivacy (\ s a -> s{_udcpAdminPrivacy = a});

-- | The name of the domain that you want to update the privacy setting for.
udcpDomainName :: Lens' UpdateDomainContactPrivacy Text
udcpDomainName = lens _udcpDomainName (\ s a -> s{_udcpDomainName = a});

instance AWSRequest UpdateDomainContactPrivacy where
        type Rs UpdateDomainContactPrivacy =
             UpdateDomainContactPrivacyResponse
        request = postJSON route53Domains
        response
          = receiveJSON
              (\ s h x ->
                 UpdateDomainContactPrivacyResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "OperationId"))

instance Hashable UpdateDomainContactPrivacy

instance NFData UpdateDomainContactPrivacy

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
              (catMaybes
                 [("TechPrivacy" .=) <$> _udcpTechPrivacy,
                  ("RegistrantPrivacy" .=) <$> _udcpRegistrantPrivacy,
                  ("AdminPrivacy" .=) <$> _udcpAdminPrivacy,
                  Just ("DomainName" .= _udcpDomainName)])

instance ToPath UpdateDomainContactPrivacy where
        toPath = const "/"

instance ToQuery UpdateDomainContactPrivacy where
        toQuery = const mempty

-- | The UpdateDomainContactPrivacy response includes the following element.
--
--
--
-- /See:/ 'updateDomainContactPrivacyResponse' smart constructor.
data UpdateDomainContactPrivacyResponse = UpdateDomainContactPrivacyResponse'
    { _udcprsResponseStatus :: !Int
    , _udcprsOperationId    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateDomainContactPrivacyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udcprsResponseStatus' - -- | The response status code.
--
-- * 'udcprsOperationId' - Identifier for tracking the progress of the request. To use this ID to query the operation status, use GetOperationDetail.
updateDomainContactPrivacyResponse
    :: Int -- ^ 'udcprsResponseStatus'
    -> Text -- ^ 'udcprsOperationId'
    -> UpdateDomainContactPrivacyResponse
updateDomainContactPrivacyResponse pResponseStatus_ pOperationId_ =
    UpdateDomainContactPrivacyResponse'
    { _udcprsResponseStatus = pResponseStatus_
    , _udcprsOperationId = pOperationId_
    }

-- | -- | The response status code.
udcprsResponseStatus :: Lens' UpdateDomainContactPrivacyResponse Int
udcprsResponseStatus = lens _udcprsResponseStatus (\ s a -> s{_udcprsResponseStatus = a});

-- | Identifier for tracking the progress of the request. To use this ID to query the operation status, use GetOperationDetail.
udcprsOperationId :: Lens' UpdateDomainContactPrivacyResponse Text
udcprsOperationId = lens _udcprsOperationId (\ s a -> s{_udcprsOperationId = a});

instance NFData UpdateDomainContactPrivacyResponse
