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
-- Module      : Network.AWS.Route53Domains.CheckDomainAvailability
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation checks the availability of one domain name. Note that if the availability status of a domain is pending, you must submit another request to determine the availability of the domain name.
--
--
module Network.AWS.Route53Domains.CheckDomainAvailability
    (
    -- * Creating a Request
      checkDomainAvailability
    , CheckDomainAvailability
    -- * Request Lenses
    , cdaIdNLangCode
    , cdaDomainName

    -- * Destructuring the Response
    , checkDomainAvailabilityResponse
    , CheckDomainAvailabilityResponse
    -- * Response Lenses
    , cdarsResponseStatus
    , cdarsAvailability
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53Domains.Types
import Network.AWS.Route53Domains.Types.Product

-- | The CheckDomainAvailability request contains the following elements.
--
--
--
-- /See:/ 'checkDomainAvailability' smart constructor.
data CheckDomainAvailability = CheckDomainAvailability'
  { _cdaIdNLangCode :: !(Maybe Text)
  , _cdaDomainName  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CheckDomainAvailability' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdaIdNLangCode' - Reserved for future use.
--
-- * 'cdaDomainName' - The name of the domain that you want to get availability for. Constraints: The domain name can contain only the letters a through z, the numbers 0 through 9, and hyphen (-). Internationalized Domain Names are not supported.
checkDomainAvailability
    :: Text -- ^ 'cdaDomainName'
    -> CheckDomainAvailability
checkDomainAvailability pDomainName_ =
  CheckDomainAvailability'
    {_cdaIdNLangCode = Nothing, _cdaDomainName = pDomainName_}


-- | Reserved for future use.
cdaIdNLangCode :: Lens' CheckDomainAvailability (Maybe Text)
cdaIdNLangCode = lens _cdaIdNLangCode (\ s a -> s{_cdaIdNLangCode = a})

-- | The name of the domain that you want to get availability for. Constraints: The domain name can contain only the letters a through z, the numbers 0 through 9, and hyphen (-). Internationalized Domain Names are not supported.
cdaDomainName :: Lens' CheckDomainAvailability Text
cdaDomainName = lens _cdaDomainName (\ s a -> s{_cdaDomainName = a})

instance AWSRequest CheckDomainAvailability where
        type Rs CheckDomainAvailability =
             CheckDomainAvailabilityResponse
        request = postJSON route53Domains
        response
          = receiveJSON
              (\ s h x ->
                 CheckDomainAvailabilityResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "Availability"))

instance Hashable CheckDomainAvailability where

instance NFData CheckDomainAvailability where

instance ToHeaders CheckDomainAvailability where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Route53Domains_v20140515.CheckDomainAvailability"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CheckDomainAvailability where
        toJSON CheckDomainAvailability'{..}
          = object
              (catMaybes
                 [("IdnLangCode" .=) <$> _cdaIdNLangCode,
                  Just ("DomainName" .= _cdaDomainName)])

instance ToPath CheckDomainAvailability where
        toPath = const "/"

instance ToQuery CheckDomainAvailability where
        toQuery = const mempty

-- | The CheckDomainAvailability response includes the following elements.
--
--
--
-- /See:/ 'checkDomainAvailabilityResponse' smart constructor.
data CheckDomainAvailabilityResponse = CheckDomainAvailabilityResponse'
  { _cdarsResponseStatus :: !Int
  , _cdarsAvailability   :: !DomainAvailability
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CheckDomainAvailabilityResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdarsResponseStatus' - -- | The response status code.
--
-- * 'cdarsAvailability' - Whether the domain name is available for registering. Valid values:     * AVAILABLE    * The domain name is available.     * AVAILABLE_RESERVED    * The domain name is reserved under specific conditions.     * AVAILABLE_PREORDER    * The domain name is available and can be preordered.     * DONT_KNOW    * The TLD registry didn't reply with a definitive answer about whether the domain name is available. Amazon Route 53 can return this response for a variety of reasons, for example, the registry is performing maintenance. Try again later.     * PENDING    * The TLD registry didn't return a response in the expected amount of time. When the response is delayed, it usually takes just a few extra seconds. You can resubmit the request immediately.     * RESERVED    * The domain name has been reserved for another person or organization.     * UNAVAILABLE    * The domain name is not available.     * UNAVAILABLE_PREMIUM    * The domain name is not available.     * UNAVAILABLE_RESTRICTED    * The domain name is forbidden.
checkDomainAvailabilityResponse
    :: Int -- ^ 'cdarsResponseStatus'
    -> DomainAvailability -- ^ 'cdarsAvailability'
    -> CheckDomainAvailabilityResponse
checkDomainAvailabilityResponse pResponseStatus_ pAvailability_ =
  CheckDomainAvailabilityResponse'
    { _cdarsResponseStatus = pResponseStatus_
    , _cdarsAvailability = pAvailability_
    }


-- | -- | The response status code.
cdarsResponseStatus :: Lens' CheckDomainAvailabilityResponse Int
cdarsResponseStatus = lens _cdarsResponseStatus (\ s a -> s{_cdarsResponseStatus = a})

-- | Whether the domain name is available for registering. Valid values:     * AVAILABLE    * The domain name is available.     * AVAILABLE_RESERVED    * The domain name is reserved under specific conditions.     * AVAILABLE_PREORDER    * The domain name is available and can be preordered.     * DONT_KNOW    * The TLD registry didn't reply with a definitive answer about whether the domain name is available. Amazon Route 53 can return this response for a variety of reasons, for example, the registry is performing maintenance. Try again later.     * PENDING    * The TLD registry didn't return a response in the expected amount of time. When the response is delayed, it usually takes just a few extra seconds. You can resubmit the request immediately.     * RESERVED    * The domain name has been reserved for another person or organization.     * UNAVAILABLE    * The domain name is not available.     * UNAVAILABLE_PREMIUM    * The domain name is not available.     * UNAVAILABLE_RESTRICTED    * The domain name is forbidden.
cdarsAvailability :: Lens' CheckDomainAvailabilityResponse DomainAvailability
cdarsAvailability = lens _cdarsAvailability (\ s a -> s{_cdarsAvailability = a})

instance NFData CheckDomainAvailabilityResponse where
