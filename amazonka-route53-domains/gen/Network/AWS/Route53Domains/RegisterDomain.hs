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
-- Module      : Network.AWS.Route53Domains.RegisterDomain
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation registers a domain. Domains are registered either by Amazon Registrar (for .com, .net, and .org domains) or by our registrar associate, Gandi (for all other domains). For some top-level domains (TLDs), this operation requires extra parameters.
--
--
-- When you register a domain, Amazon Route 53 does the following:
--
--     * Creates a Amazon Route 53 hosted zone that has the same name as the domain. Amazon Route 53 assigns four name servers to your hosted zone and automatically updates your domain registration with the names of these name servers.
--
--     * Enables autorenew, so your domain registration will renew automatically each year. We'll notify you in advance of the renewal date so you can choose whether to renew the registration.
--
--     * Optionally enables privacy protection, so WHOIS queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If you don't enable privacy protection, WHOIS queries return the information that you entered for the registrant, admin, and tech contacts.
--
--     * If registration is successful, returns an operation ID that you can use to track the progress and completion of the action. If the request is not completed successfully, the domain registrant is notified by email.
--
--     * Charges your AWS account an amount based on the top-level domain. For more information, see <http://aws.amazon.com/route53/pricing/ Amazon Route 53 Pricing> .
--
--
--
module Network.AWS.Route53Domains.RegisterDomain
    (
    -- * Creating a Request
      registerDomain
    , RegisterDomain
    -- * Request Lenses
    , rPrivacyProtectTechContact
    , rPrivacyProtectRegistrantContact
    , rAutoRenew
    , rPrivacyProtectAdminContact
    , rIdNLangCode
    , rDomainName
    , rDurationInYears
    , rAdminContact
    , rRegistrantContact
    , rTechContact

    -- * Destructuring the Response
    , registerDomainResponse
    , RegisterDomainResponse
    -- * Response Lenses
    , rdrsResponseStatus
    , rdrsOperationId
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53Domains.Types
import Network.AWS.Route53Domains.Types.Product

-- | The RegisterDomain request includes the following elements.
--
--
--
-- /See:/ 'registerDomain' smart constructor.
data RegisterDomain = RegisterDomain'
  { _rPrivacyProtectTechContact       :: !(Maybe Bool)
  , _rPrivacyProtectRegistrantContact :: !(Maybe Bool)
  , _rAutoRenew                       :: !(Maybe Bool)
  , _rPrivacyProtectAdminContact      :: !(Maybe Bool)
  , _rIdNLangCode                     :: !(Maybe Text)
  , _rDomainName                      :: !Text
  , _rDurationInYears                 :: !Nat
  , _rAdminContact                    :: !(Sensitive ContactDetail)
  , _rRegistrantContact               :: !(Sensitive ContactDetail)
  , _rTechContact                     :: !(Sensitive ContactDetail)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'RegisterDomain' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rPrivacyProtectTechContact' - Whether you want to conceal contact information from WHOIS queries. If you specify @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If you specify @false@ , WHOIS queries return the information that you entered for the technical contact. Default: @true@
--
-- * 'rPrivacyProtectRegistrantContact' - Whether you want to conceal contact information from WHOIS queries. If you specify @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If you specify @false@ , WHOIS queries return the information that you entered for the registrant contact (the domain owner). Default: @true@
--
-- * 'rAutoRenew' - Indicates whether the domain will be automatically renewed (@true@ ) or not (@false@ ). Autorenewal only takes effect after the account is charged. Default: @true@
--
-- * 'rPrivacyProtectAdminContact' - Whether you want to conceal contact information from WHOIS queries. If you specify @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If you specify @false@ , WHOIS queries return the information that you entered for the admin contact. Default: @true@
--
-- * 'rIdNLangCode' - Reserved for future use.
--
-- * 'rDomainName' - The domain name that you want to register. Constraints: The domain name can contain only the letters a through z, the numbers 0 through 9, and hyphen (-). Internationalized Domain Names are not supported.
--
-- * 'rDurationInYears' - The number of years that you want to register the domain for. Domains are registered for a minimum of one year. The maximum period depends on the top-level domain. For the range of valid values for your domain, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53> in the /Amazon Route 53 Developer Guide/ . Default: 1
--
-- * 'rAdminContact' - Provides detailed contact information.
--
-- * 'rRegistrantContact' - Provides detailed contact information.
--
-- * 'rTechContact' - Provides detailed contact information.
registerDomain
    :: Text -- ^ 'rDomainName'
    -> Natural -- ^ 'rDurationInYears'
    -> ContactDetail -- ^ 'rAdminContact'
    -> ContactDetail -- ^ 'rRegistrantContact'
    -> ContactDetail -- ^ 'rTechContact'
    -> RegisterDomain
registerDomain pDomainName_ pDurationInYears_ pAdminContact_ pRegistrantContact_ pTechContact_ =
  RegisterDomain'
    { _rPrivacyProtectTechContact = Nothing
    , _rPrivacyProtectRegistrantContact = Nothing
    , _rAutoRenew = Nothing
    , _rPrivacyProtectAdminContact = Nothing
    , _rIdNLangCode = Nothing
    , _rDomainName = pDomainName_
    , _rDurationInYears = _Nat # pDurationInYears_
    , _rAdminContact = _Sensitive # pAdminContact_
    , _rRegistrantContact = _Sensitive # pRegistrantContact_
    , _rTechContact = _Sensitive # pTechContact_
    }


-- | Whether you want to conceal contact information from WHOIS queries. If you specify @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If you specify @false@ , WHOIS queries return the information that you entered for the technical contact. Default: @true@
rPrivacyProtectTechContact :: Lens' RegisterDomain (Maybe Bool)
rPrivacyProtectTechContact = lens _rPrivacyProtectTechContact (\ s a -> s{_rPrivacyProtectTechContact = a})

-- | Whether you want to conceal contact information from WHOIS queries. If you specify @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If you specify @false@ , WHOIS queries return the information that you entered for the registrant contact (the domain owner). Default: @true@
rPrivacyProtectRegistrantContact :: Lens' RegisterDomain (Maybe Bool)
rPrivacyProtectRegistrantContact = lens _rPrivacyProtectRegistrantContact (\ s a -> s{_rPrivacyProtectRegistrantContact = a})

-- | Indicates whether the domain will be automatically renewed (@true@ ) or not (@false@ ). Autorenewal only takes effect after the account is charged. Default: @true@
rAutoRenew :: Lens' RegisterDomain (Maybe Bool)
rAutoRenew = lens _rAutoRenew (\ s a -> s{_rAutoRenew = a})

-- | Whether you want to conceal contact information from WHOIS queries. If you specify @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If you specify @false@ , WHOIS queries return the information that you entered for the admin contact. Default: @true@
rPrivacyProtectAdminContact :: Lens' RegisterDomain (Maybe Bool)
rPrivacyProtectAdminContact = lens _rPrivacyProtectAdminContact (\ s a -> s{_rPrivacyProtectAdminContact = a})

-- | Reserved for future use.
rIdNLangCode :: Lens' RegisterDomain (Maybe Text)
rIdNLangCode = lens _rIdNLangCode (\ s a -> s{_rIdNLangCode = a})

-- | The domain name that you want to register. Constraints: The domain name can contain only the letters a through z, the numbers 0 through 9, and hyphen (-). Internationalized Domain Names are not supported.
rDomainName :: Lens' RegisterDomain Text
rDomainName = lens _rDomainName (\ s a -> s{_rDomainName = a})

-- | The number of years that you want to register the domain for. Domains are registered for a minimum of one year. The maximum period depends on the top-level domain. For the range of valid values for your domain, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53> in the /Amazon Route 53 Developer Guide/ . Default: 1
rDurationInYears :: Lens' RegisterDomain Natural
rDurationInYears = lens _rDurationInYears (\ s a -> s{_rDurationInYears = a}) . _Nat

-- | Provides detailed contact information.
rAdminContact :: Lens' RegisterDomain ContactDetail
rAdminContact = lens _rAdminContact (\ s a -> s{_rAdminContact = a}) . _Sensitive

-- | Provides detailed contact information.
rRegistrantContact :: Lens' RegisterDomain ContactDetail
rRegistrantContact = lens _rRegistrantContact (\ s a -> s{_rRegistrantContact = a}) . _Sensitive

-- | Provides detailed contact information.
rTechContact :: Lens' RegisterDomain ContactDetail
rTechContact = lens _rTechContact (\ s a -> s{_rTechContact = a}) . _Sensitive

instance AWSRequest RegisterDomain where
        type Rs RegisterDomain = RegisterDomainResponse
        request = postJSON route53Domains
        response
          = receiveJSON
              (\ s h x ->
                 RegisterDomainResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "OperationId"))

instance Hashable RegisterDomain where

instance NFData RegisterDomain where

instance ToHeaders RegisterDomain where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Route53Domains_v20140515.RegisterDomain" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RegisterDomain where
        toJSON RegisterDomain'{..}
          = object
              (catMaybes
                 [("PrivacyProtectTechContact" .=) <$>
                    _rPrivacyProtectTechContact,
                  ("PrivacyProtectRegistrantContact" .=) <$>
                    _rPrivacyProtectRegistrantContact,
                  ("AutoRenew" .=) <$> _rAutoRenew,
                  ("PrivacyProtectAdminContact" .=) <$>
                    _rPrivacyProtectAdminContact,
                  ("IdnLangCode" .=) <$> _rIdNLangCode,
                  Just ("DomainName" .= _rDomainName),
                  Just ("DurationInYears" .= _rDurationInYears),
                  Just ("AdminContact" .= _rAdminContact),
                  Just ("RegistrantContact" .= _rRegistrantContact),
                  Just ("TechContact" .= _rTechContact)])

instance ToPath RegisterDomain where
        toPath = const "/"

instance ToQuery RegisterDomain where
        toQuery = const mempty

-- | The RegisterDomain response includes the following element.
--
--
--
-- /See:/ 'registerDomainResponse' smart constructor.
data RegisterDomainResponse = RegisterDomainResponse'
  { _rdrsResponseStatus :: !Int
  , _rdrsOperationId    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RegisterDomainResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdrsResponseStatus' - -- | The response status code.
--
-- * 'rdrsOperationId' - Identifier for tracking the progress of the request. To use this ID to query the operation status, use 'GetOperationDetail' .
registerDomainResponse
    :: Int -- ^ 'rdrsResponseStatus'
    -> Text -- ^ 'rdrsOperationId'
    -> RegisterDomainResponse
registerDomainResponse pResponseStatus_ pOperationId_ =
  RegisterDomainResponse'
    {_rdrsResponseStatus = pResponseStatus_, _rdrsOperationId = pOperationId_}


-- | -- | The response status code.
rdrsResponseStatus :: Lens' RegisterDomainResponse Int
rdrsResponseStatus = lens _rdrsResponseStatus (\ s a -> s{_rdrsResponseStatus = a})

-- | Identifier for tracking the progress of the request. To use this ID to query the operation status, use 'GetOperationDetail' .
rdrsOperationId :: Lens' RegisterDomainResponse Text
rdrsOperationId = lens _rdrsOperationId (\ s a -> s{_rdrsOperationId = a})

instance NFData RegisterDomainResponse where
