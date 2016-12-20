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
-- Module      : Network.AWS.Route53Domains.TransferDomain
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation transfers a domain from another registrar to Amazon Route 53. When the transfer is complete, the domain is registered with the AWS registrar partner, Gandi.
--
--
-- For transfer requirements, a detailed procedure, and information about viewing the status of a domain transfer, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/domain-transfer-to-route-53.html Transferring Registration for a Domain to Amazon Route 53> in the Amazon Route 53 Developer Guide.
--
-- If the registrar for your domain is also the DNS service provider for the domain, we highly recommend that you consider transferring your DNS service to Amazon Route 53 or to another DNS service provider before you transfer your registration. Some registrars provide free DNS service when you purchase a domain registration. When you transfer the registration, the previous registrar will not renew your domain registration and could end your DNS service at any time.
--
-- If the transfer is successful, this method returns an operation ID that you can use to track the progress and completion of the action. If the transfer doesn't complete successfully, the domain registrant will be notified by email.
--
module Network.AWS.Route53Domains.TransferDomain
    (
    -- * Creating a Request
      transferDomain
    , TransferDomain
    -- * Request Lenses
    , tdPrivacyProtectTechContact
    , tdPrivacyProtectRegistrantContact
    , tdAutoRenew
    , tdPrivacyProtectAdminContact
    , tdIdNLangCode
    , tdAuthCode
    , tdNameservers
    , tdDomainName
    , tdDurationInYears
    , tdAdminContact
    , tdRegistrantContact
    , tdTechContact

    -- * Destructuring the Response
    , transferDomainResponse
    , TransferDomainResponse
    -- * Response Lenses
    , tdrsResponseStatus
    , tdrsOperationId
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53Domains.Types
import           Network.AWS.Route53Domains.Types.Product

-- | The TransferDomain request includes the following elements.
--
--
--
-- /See:/ 'transferDomain' smart constructor.
data TransferDomain = TransferDomain'
    { _tdPrivacyProtectTechContact       :: !(Maybe Bool)
    , _tdPrivacyProtectRegistrantContact :: !(Maybe Bool)
    , _tdAutoRenew                       :: !(Maybe Bool)
    , _tdPrivacyProtectAdminContact      :: !(Maybe Bool)
    , _tdIdNLangCode                     :: !(Maybe Text)
    , _tdAuthCode                        :: !(Maybe (Sensitive Text))
    , _tdNameservers                     :: !(Maybe [Nameserver])
    , _tdDomainName                      :: !Text
    , _tdDurationInYears                 :: !Nat
    , _tdAdminContact                    :: !(Sensitive ContactDetail)
    , _tdRegistrantContact               :: !(Sensitive ContactDetail)
    , _tdTechContact                     :: !(Sensitive ContactDetail)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'TransferDomain' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tdPrivacyProtectTechContact' - Whether you want to conceal contact information from WHOIS queries. If you specify true, WHOIS ("who is") queries will return contact information for our registrar partner, Gandi, instead of the contact information that you enter. Type: Boolean Default: @true@  Valid values: @true@ | @false@  Required: No
--
-- * 'tdPrivacyProtectRegistrantContact' - Whether you want to conceal contact information from WHOIS queries. If you specify true, WHOIS ("who is") queries will return contact information for our registrar partner, Gandi, instead of the contact information that you enter. Type: Boolean Default: @true@  Valid values: @true@ | @false@  Required: No
--
-- * 'tdAutoRenew' - Indicates whether the domain will be automatically renewed (true) or not (false). Autorenewal only takes effect after the account is charged. Type: Boolean Valid values: @true@ | @false@  Default: true Required: No
--
-- * 'tdPrivacyProtectAdminContact' - Whether you want to conceal contact information from WHOIS queries. If you specify true, WHOIS ("who is") queries will return contact information for our registrar partner, Gandi, instead of the contact information that you enter. Type: Boolean Default: @true@  Valid values: @true@ | @false@  Required: No
--
-- * 'tdIdNLangCode' - Reserved for future use.
--
-- * 'tdAuthCode' - The authorization code for the domain. You get this value from the current registrar. Type: String Required: Yes
--
-- * 'tdNameservers' - Contains details for the host and glue IP addresses. Type: Complex Children: @GlueIps@ , @Name@  Required: No
--
-- * 'tdDomainName' - The name of a domain. Type: String Default: None Constraints: The domain name can contain only the letters a through z, the numbers 0 through 9, and hyphen (-). Internationalized Domain Names are not supported. Required: Yes
--
-- * 'tdDurationInYears' - The number of years the domain will be registered. Domains are registered for a minimum of one year. The maximum period depends on the top-level domain. Type: Integer Default: 1 Valid values: Integer from 1 to 10 Required: Yes
--
-- * 'tdAdminContact' - Provides detailed contact information. Type: Complex Children: @FirstName@ , @MiddleName@ , @LastName@ , @ContactType@ , @OrganizationName@ , @AddressLine1@ , @AddressLine2@ , @City@ , @State@ , @CountryCode@ , @ZipCode@ , @PhoneNumber@ , @Email@ , @Fax@ , @ExtraParams@  Required: Yes
--
-- * 'tdRegistrantContact' - Provides detailed contact information. Type: Complex Children: @FirstName@ , @MiddleName@ , @LastName@ , @ContactType@ , @OrganizationName@ , @AddressLine1@ , @AddressLine2@ , @City@ , @State@ , @CountryCode@ , @ZipCode@ , @PhoneNumber@ , @Email@ , @Fax@ , @ExtraParams@  Required: Yes
--
-- * 'tdTechContact' - Provides detailed contact information. Type: Complex Children: @FirstName@ , @MiddleName@ , @LastName@ , @ContactType@ , @OrganizationName@ , @AddressLine1@ , @AddressLine2@ , @City@ , @State@ , @CountryCode@ , @ZipCode@ , @PhoneNumber@ , @Email@ , @Fax@ , @ExtraParams@  Required: Yes
transferDomain
    :: Text -- ^ 'tdDomainName'
    -> Natural -- ^ 'tdDurationInYears'
    -> ContactDetail -- ^ 'tdAdminContact'
    -> ContactDetail -- ^ 'tdRegistrantContact'
    -> ContactDetail -- ^ 'tdTechContact'
    -> TransferDomain
transferDomain pDomainName_ pDurationInYears_ pAdminContact_ pRegistrantContact_ pTechContact_ =
    TransferDomain'
    { _tdPrivacyProtectTechContact = Nothing
    , _tdPrivacyProtectRegistrantContact = Nothing
    , _tdAutoRenew = Nothing
    , _tdPrivacyProtectAdminContact = Nothing
    , _tdIdNLangCode = Nothing
    , _tdAuthCode = Nothing
    , _tdNameservers = Nothing
    , _tdDomainName = pDomainName_
    , _tdDurationInYears = _Nat # pDurationInYears_
    , _tdAdminContact = _Sensitive # pAdminContact_
    , _tdRegistrantContact = _Sensitive # pRegistrantContact_
    , _tdTechContact = _Sensitive # pTechContact_
    }

-- | Whether you want to conceal contact information from WHOIS queries. If you specify true, WHOIS ("who is") queries will return contact information for our registrar partner, Gandi, instead of the contact information that you enter. Type: Boolean Default: @true@  Valid values: @true@ | @false@  Required: No
tdPrivacyProtectTechContact :: Lens' TransferDomain (Maybe Bool)
tdPrivacyProtectTechContact = lens _tdPrivacyProtectTechContact (\ s a -> s{_tdPrivacyProtectTechContact = a});

-- | Whether you want to conceal contact information from WHOIS queries. If you specify true, WHOIS ("who is") queries will return contact information for our registrar partner, Gandi, instead of the contact information that you enter. Type: Boolean Default: @true@  Valid values: @true@ | @false@  Required: No
tdPrivacyProtectRegistrantContact :: Lens' TransferDomain (Maybe Bool)
tdPrivacyProtectRegistrantContact = lens _tdPrivacyProtectRegistrantContact (\ s a -> s{_tdPrivacyProtectRegistrantContact = a});

-- | Indicates whether the domain will be automatically renewed (true) or not (false). Autorenewal only takes effect after the account is charged. Type: Boolean Valid values: @true@ | @false@  Default: true Required: No
tdAutoRenew :: Lens' TransferDomain (Maybe Bool)
tdAutoRenew = lens _tdAutoRenew (\ s a -> s{_tdAutoRenew = a});

-- | Whether you want to conceal contact information from WHOIS queries. If you specify true, WHOIS ("who is") queries will return contact information for our registrar partner, Gandi, instead of the contact information that you enter. Type: Boolean Default: @true@  Valid values: @true@ | @false@  Required: No
tdPrivacyProtectAdminContact :: Lens' TransferDomain (Maybe Bool)
tdPrivacyProtectAdminContact = lens _tdPrivacyProtectAdminContact (\ s a -> s{_tdPrivacyProtectAdminContact = a});

-- | Reserved for future use.
tdIdNLangCode :: Lens' TransferDomain (Maybe Text)
tdIdNLangCode = lens _tdIdNLangCode (\ s a -> s{_tdIdNLangCode = a});

-- | The authorization code for the domain. You get this value from the current registrar. Type: String Required: Yes
tdAuthCode :: Lens' TransferDomain (Maybe Text)
tdAuthCode = lens _tdAuthCode (\ s a -> s{_tdAuthCode = a}) . mapping _Sensitive;

-- | Contains details for the host and glue IP addresses. Type: Complex Children: @GlueIps@ , @Name@  Required: No
tdNameservers :: Lens' TransferDomain [Nameserver]
tdNameservers = lens _tdNameservers (\ s a -> s{_tdNameservers = a}) . _Default . _Coerce;

-- | The name of a domain. Type: String Default: None Constraints: The domain name can contain only the letters a through z, the numbers 0 through 9, and hyphen (-). Internationalized Domain Names are not supported. Required: Yes
tdDomainName :: Lens' TransferDomain Text
tdDomainName = lens _tdDomainName (\ s a -> s{_tdDomainName = a});

-- | The number of years the domain will be registered. Domains are registered for a minimum of one year. The maximum period depends on the top-level domain. Type: Integer Default: 1 Valid values: Integer from 1 to 10 Required: Yes
tdDurationInYears :: Lens' TransferDomain Natural
tdDurationInYears = lens _tdDurationInYears (\ s a -> s{_tdDurationInYears = a}) . _Nat;

-- | Provides detailed contact information. Type: Complex Children: @FirstName@ , @MiddleName@ , @LastName@ , @ContactType@ , @OrganizationName@ , @AddressLine1@ , @AddressLine2@ , @City@ , @State@ , @CountryCode@ , @ZipCode@ , @PhoneNumber@ , @Email@ , @Fax@ , @ExtraParams@  Required: Yes
tdAdminContact :: Lens' TransferDomain ContactDetail
tdAdminContact = lens _tdAdminContact (\ s a -> s{_tdAdminContact = a}) . _Sensitive;

-- | Provides detailed contact information. Type: Complex Children: @FirstName@ , @MiddleName@ , @LastName@ , @ContactType@ , @OrganizationName@ , @AddressLine1@ , @AddressLine2@ , @City@ , @State@ , @CountryCode@ , @ZipCode@ , @PhoneNumber@ , @Email@ , @Fax@ , @ExtraParams@  Required: Yes
tdRegistrantContact :: Lens' TransferDomain ContactDetail
tdRegistrantContact = lens _tdRegistrantContact (\ s a -> s{_tdRegistrantContact = a}) . _Sensitive;

-- | Provides detailed contact information. Type: Complex Children: @FirstName@ , @MiddleName@ , @LastName@ , @ContactType@ , @OrganizationName@ , @AddressLine1@ , @AddressLine2@ , @City@ , @State@ , @CountryCode@ , @ZipCode@ , @PhoneNumber@ , @Email@ , @Fax@ , @ExtraParams@  Required: Yes
tdTechContact :: Lens' TransferDomain ContactDetail
tdTechContact = lens _tdTechContact (\ s a -> s{_tdTechContact = a}) . _Sensitive;

instance AWSRequest TransferDomain where
        type Rs TransferDomain = TransferDomainResponse
        request = postJSON route53Domains
        response
          = receiveJSON
              (\ s h x ->
                 TransferDomainResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "OperationId"))

instance Hashable TransferDomain

instance NFData TransferDomain

instance ToHeaders TransferDomain where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Route53Domains_v20140515.TransferDomain" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON TransferDomain where
        toJSON TransferDomain'{..}
          = object
              (catMaybes
                 [("PrivacyProtectTechContact" .=) <$>
                    _tdPrivacyProtectTechContact,
                  ("PrivacyProtectRegistrantContact" .=) <$>
                    _tdPrivacyProtectRegistrantContact,
                  ("AutoRenew" .=) <$> _tdAutoRenew,
                  ("PrivacyProtectAdminContact" .=) <$>
                    _tdPrivacyProtectAdminContact,
                  ("IdnLangCode" .=) <$> _tdIdNLangCode,
                  ("AuthCode" .=) <$> _tdAuthCode,
                  ("Nameservers" .=) <$> _tdNameservers,
                  Just ("DomainName" .= _tdDomainName),
                  Just ("DurationInYears" .= _tdDurationInYears),
                  Just ("AdminContact" .= _tdAdminContact),
                  Just ("RegistrantContact" .= _tdRegistrantContact),
                  Just ("TechContact" .= _tdTechContact)])

instance ToPath TransferDomain where
        toPath = const "/"

instance ToQuery TransferDomain where
        toQuery = const mempty

-- | The TranserDomain response includes the following element.
--
--
--
-- /See:/ 'transferDomainResponse' smart constructor.
data TransferDomainResponse = TransferDomainResponse'
    { _tdrsResponseStatus :: !Int
    , _tdrsOperationId    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'TransferDomainResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tdrsResponseStatus' - -- | The response status code.
--
-- * 'tdrsOperationId' - Identifier for tracking the progress of the request. To use this ID to query the operation status, use GetOperationDetail. Type: String Default: None Constraints: Maximum 255 characters.
transferDomainResponse
    :: Int -- ^ 'tdrsResponseStatus'
    -> Text -- ^ 'tdrsOperationId'
    -> TransferDomainResponse
transferDomainResponse pResponseStatus_ pOperationId_ =
    TransferDomainResponse'
    { _tdrsResponseStatus = pResponseStatus_
    , _tdrsOperationId = pOperationId_
    }

-- | -- | The response status code.
tdrsResponseStatus :: Lens' TransferDomainResponse Int
tdrsResponseStatus = lens _tdrsResponseStatus (\ s a -> s{_tdrsResponseStatus = a});

-- | Identifier for tracking the progress of the request. To use this ID to query the operation status, use GetOperationDetail. Type: String Default: None Constraints: Maximum 255 characters.
tdrsOperationId :: Lens' TransferDomainResponse Text
tdrsOperationId = lens _tdrsOperationId (\ s a -> s{_tdrsOperationId = a});

instance NFData TransferDomainResponse
