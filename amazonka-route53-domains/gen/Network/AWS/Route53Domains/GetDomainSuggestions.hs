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
-- Module      : Network.AWS.Route53Domains.GetDomainSuggestions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The GetDomainSuggestions operation returns a list of suggested domain names given a string, which can either be a domain name or simply a word or phrase (without spaces).
--
--
module Network.AWS.Route53Domains.GetDomainSuggestions
    (
    -- * Creating a Request
      getDomainSuggestions
    , GetDomainSuggestions
    -- * Request Lenses
    , gdsDomainName
    , gdsSuggestionCount
    , gdsOnlyAvailable

    -- * Destructuring the Response
    , getDomainSuggestionsResponse
    , GetDomainSuggestionsResponse
    -- * Response Lenses
    , gdsrsSuggestionsList
    , gdsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53Domains.Types
import Network.AWS.Route53Domains.Types.Product

-- | /See:/ 'getDomainSuggestions' smart constructor.
data GetDomainSuggestions = GetDomainSuggestions'
  { _gdsDomainName      :: !Text
  , _gdsSuggestionCount :: !Int
  , _gdsOnlyAvailable   :: !Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDomainSuggestions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdsDomainName' - A domain name that you want to use as the basis for a list of possible domain names. The domain name must contain a top-level domain (TLD), such as .com, that Amazon Route 53 supports. For a list of TLDs, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53> in the /Amazon Route 53 Developer Guide/ .
--
-- * 'gdsSuggestionCount' - The number of suggested domain names that you want Amazon Route 53 to return.
--
-- * 'gdsOnlyAvailable' - If @OnlyAvailable@ is @true@ , Amazon Route 53 returns only domain names that are available. If @OnlyAvailable@ is @false@ , Amazon Route 53 returns domain names without checking whether they're available to be registered. To determine whether the domain is available, you can call @checkDomainAvailability@ for each suggestion.
getDomainSuggestions
    :: Text -- ^ 'gdsDomainName'
    -> Int -- ^ 'gdsSuggestionCount'
    -> Bool -- ^ 'gdsOnlyAvailable'
    -> GetDomainSuggestions
getDomainSuggestions pDomainName_ pSuggestionCount_ pOnlyAvailable_ =
  GetDomainSuggestions'
    { _gdsDomainName = pDomainName_
    , _gdsSuggestionCount = pSuggestionCount_
    , _gdsOnlyAvailable = pOnlyAvailable_
    }


-- | A domain name that you want to use as the basis for a list of possible domain names. The domain name must contain a top-level domain (TLD), such as .com, that Amazon Route 53 supports. For a list of TLDs, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53> in the /Amazon Route 53 Developer Guide/ .
gdsDomainName :: Lens' GetDomainSuggestions Text
gdsDomainName = lens _gdsDomainName (\ s a -> s{_gdsDomainName = a})

-- | The number of suggested domain names that you want Amazon Route 53 to return.
gdsSuggestionCount :: Lens' GetDomainSuggestions Int
gdsSuggestionCount = lens _gdsSuggestionCount (\ s a -> s{_gdsSuggestionCount = a})

-- | If @OnlyAvailable@ is @true@ , Amazon Route 53 returns only domain names that are available. If @OnlyAvailable@ is @false@ , Amazon Route 53 returns domain names without checking whether they're available to be registered. To determine whether the domain is available, you can call @checkDomainAvailability@ for each suggestion.
gdsOnlyAvailable :: Lens' GetDomainSuggestions Bool
gdsOnlyAvailable = lens _gdsOnlyAvailable (\ s a -> s{_gdsOnlyAvailable = a})

instance AWSRequest GetDomainSuggestions where
        type Rs GetDomainSuggestions =
             GetDomainSuggestionsResponse
        request = postJSON route53Domains
        response
          = receiveJSON
              (\ s h x ->
                 GetDomainSuggestionsResponse' <$>
                   (x .?> "SuggestionsList" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable GetDomainSuggestions where

instance NFData GetDomainSuggestions where

instance ToHeaders GetDomainSuggestions where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Route53Domains_v20140515.GetDomainSuggestions" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetDomainSuggestions where
        toJSON GetDomainSuggestions'{..}
          = object
              (catMaybes
                 [Just ("DomainName" .= _gdsDomainName),
                  Just ("SuggestionCount" .= _gdsSuggestionCount),
                  Just ("OnlyAvailable" .= _gdsOnlyAvailable)])

instance ToPath GetDomainSuggestions where
        toPath = const "/"

instance ToQuery GetDomainSuggestions where
        toQuery = const mempty

-- | /See:/ 'getDomainSuggestionsResponse' smart constructor.
data GetDomainSuggestionsResponse = GetDomainSuggestionsResponse'
  { _gdsrsSuggestionsList :: !(Maybe [DomainSuggestion])
  , _gdsrsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDomainSuggestionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdsrsSuggestionsList' - A list of possible domain names. If you specified @true@ for @OnlyAvailable@ in the request, the list contains only domains that are available for registration.
--
-- * 'gdsrsResponseStatus' - -- | The response status code.
getDomainSuggestionsResponse
    :: Int -- ^ 'gdsrsResponseStatus'
    -> GetDomainSuggestionsResponse
getDomainSuggestionsResponse pResponseStatus_ =
  GetDomainSuggestionsResponse'
    {_gdsrsSuggestionsList = Nothing, _gdsrsResponseStatus = pResponseStatus_}


-- | A list of possible domain names. If you specified @true@ for @OnlyAvailable@ in the request, the list contains only domains that are available for registration.
gdsrsSuggestionsList :: Lens' GetDomainSuggestionsResponse [DomainSuggestion]
gdsrsSuggestionsList = lens _gdsrsSuggestionsList (\ s a -> s{_gdsrsSuggestionsList = a}) . _Default . _Coerce

-- | -- | The response status code.
gdsrsResponseStatus :: Lens' GetDomainSuggestionsResponse Int
gdsrsResponseStatus = lens _gdsrsResponseStatus (\ s a -> s{_gdsrsResponseStatus = a})

instance NFData GetDomainSuggestionsResponse where
