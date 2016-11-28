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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The GetDomainSuggestions operation returns a list of suggested domain names given a string, which can either be a domain name or simply a word or phrase (without spaces).
--
--
-- Parameters:     * DomainName (string): The basis for your domain suggestion search, a string with (or without) top-level domain specified.    * SuggestionCount (int): The number of domain suggestions to be returned, maximum 50, minimum 1.    * OnlyAvailable (bool): If true, availability check will be performed on suggestion results, and only available domains will be returned. If false, suggestions will be returned without checking whether the domain is actually available, and caller will have to call checkDomainAvailability for each suggestion to determine availability for registration.
--
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

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53Domains.Types
import           Network.AWS.Route53Domains.Types.Product

-- | /See:/ 'getDomainSuggestions' smart constructor.
data GetDomainSuggestions = GetDomainSuggestions'
    { _gdsDomainName      :: !Text
    , _gdsSuggestionCount :: !Int
    , _gdsOnlyAvailable   :: !Bool
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetDomainSuggestions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdsDomainName' - Undocumented member.
--
-- * 'gdsSuggestionCount' - Undocumented member.
--
-- * 'gdsOnlyAvailable' - Undocumented member.
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

-- | Undocumented member.
gdsDomainName :: Lens' GetDomainSuggestions Text
gdsDomainName = lens _gdsDomainName (\ s a -> s{_gdsDomainName = a});

-- | Undocumented member.
gdsSuggestionCount :: Lens' GetDomainSuggestions Int
gdsSuggestionCount = lens _gdsSuggestionCount (\ s a -> s{_gdsSuggestionCount = a});

-- | Undocumented member.
gdsOnlyAvailable :: Lens' GetDomainSuggestions Bool
gdsOnlyAvailable = lens _gdsOnlyAvailable (\ s a -> s{_gdsOnlyAvailable = a});

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

instance Hashable GetDomainSuggestions

instance NFData GetDomainSuggestions

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetDomainSuggestionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdsrsSuggestionsList' - Undocumented member.
--
-- * 'gdsrsResponseStatus' - -- | The response status code.
getDomainSuggestionsResponse
    :: Int -- ^ 'gdsrsResponseStatus'
    -> GetDomainSuggestionsResponse
getDomainSuggestionsResponse pResponseStatus_ =
    GetDomainSuggestionsResponse'
    { _gdsrsSuggestionsList = Nothing
    , _gdsrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
gdsrsSuggestionsList :: Lens' GetDomainSuggestionsResponse [DomainSuggestion]
gdsrsSuggestionsList = lens _gdsrsSuggestionsList (\ s a -> s{_gdsrsSuggestionsList = a}) . _Default . _Coerce;

-- | -- | The response status code.
gdsrsResponseStatus :: Lens' GetDomainSuggestionsResponse Int
gdsrsResponseStatus = lens _gdsrsResponseStatus (\ s a -> s{_gdsrsResponseStatus = a});

instance NFData GetDomainSuggestionsResponse
