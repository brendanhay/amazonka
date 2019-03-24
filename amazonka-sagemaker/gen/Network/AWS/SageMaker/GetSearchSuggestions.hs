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
-- Module      : Network.AWS.SageMaker.GetSearchSuggestions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- An auto-complete API for the search functionality in the Amazon SageMaker console. It returns suggestions of possible matches for the property name to use in @Search@ queries. Provides suggestions for @HyperParameters@ , @Tags@ , and @Metrics@ .
--
--
module Network.AWS.SageMaker.GetSearchSuggestions
    (
    -- * Creating a Request
      getSearchSuggestions
    , GetSearchSuggestions
    -- * Request Lenses
    , gssSuggestionQuery
    , gssResource

    -- * Destructuring the Response
    , getSearchSuggestionsResponse
    , GetSearchSuggestionsResponse
    -- * Response Lenses
    , gssrsPropertyNameSuggestions
    , gssrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'getSearchSuggestions' smart constructor.
data GetSearchSuggestions = GetSearchSuggestions'
  { _gssSuggestionQuery :: !(Maybe SuggestionQuery)
  , _gssResource        :: !ResourceType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetSearchSuggestions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gssSuggestionQuery' - Limits the property names that are included in the response.
--
-- * 'gssResource' - The name of the Amazon SageMaker resource to Search for. The only valid @Resource@ value is @TrainingJob@ .
getSearchSuggestions
    :: ResourceType -- ^ 'gssResource'
    -> GetSearchSuggestions
getSearchSuggestions pResource_ =
  GetSearchSuggestions'
    {_gssSuggestionQuery = Nothing, _gssResource = pResource_}


-- | Limits the property names that are included in the response.
gssSuggestionQuery :: Lens' GetSearchSuggestions (Maybe SuggestionQuery)
gssSuggestionQuery = lens _gssSuggestionQuery (\ s a -> s{_gssSuggestionQuery = a})

-- | The name of the Amazon SageMaker resource to Search for. The only valid @Resource@ value is @TrainingJob@ .
gssResource :: Lens' GetSearchSuggestions ResourceType
gssResource = lens _gssResource (\ s a -> s{_gssResource = a})

instance AWSRequest GetSearchSuggestions where
        type Rs GetSearchSuggestions =
             GetSearchSuggestionsResponse
        request = postJSON sageMaker
        response
          = receiveJSON
              (\ s h x ->
                 GetSearchSuggestionsResponse' <$>
                   (x .?> "PropertyNameSuggestions" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable GetSearchSuggestions where

instance NFData GetSearchSuggestions where

instance ToHeaders GetSearchSuggestions where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.GetSearchSuggestions" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetSearchSuggestions where
        toJSON GetSearchSuggestions'{..}
          = object
              (catMaybes
                 [("SuggestionQuery" .=) <$> _gssSuggestionQuery,
                  Just ("Resource" .= _gssResource)])

instance ToPath GetSearchSuggestions where
        toPath = const "/"

instance ToQuery GetSearchSuggestions where
        toQuery = const mempty

-- | /See:/ 'getSearchSuggestionsResponse' smart constructor.
data GetSearchSuggestionsResponse = GetSearchSuggestionsResponse'
  { _gssrsPropertyNameSuggestions :: !(Maybe [PropertyNameSuggestion])
  , _gssrsResponseStatus          :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetSearchSuggestionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gssrsPropertyNameSuggestions' - A list of property names for a @Resource@ that match a @SuggestionQuery@ .
--
-- * 'gssrsResponseStatus' - -- | The response status code.
getSearchSuggestionsResponse
    :: Int -- ^ 'gssrsResponseStatus'
    -> GetSearchSuggestionsResponse
getSearchSuggestionsResponse pResponseStatus_ =
  GetSearchSuggestionsResponse'
    { _gssrsPropertyNameSuggestions = Nothing
    , _gssrsResponseStatus = pResponseStatus_
    }


-- | A list of property names for a @Resource@ that match a @SuggestionQuery@ .
gssrsPropertyNameSuggestions :: Lens' GetSearchSuggestionsResponse [PropertyNameSuggestion]
gssrsPropertyNameSuggestions = lens _gssrsPropertyNameSuggestions (\ s a -> s{_gssrsPropertyNameSuggestions = a}) . _Default . _Coerce

-- | -- | The response status code.
gssrsResponseStatus :: Lens' GetSearchSuggestionsResponse Int
gssrsResponseStatus = lens _gssrsResponseStatus (\ s a -> s{_gssrsResponseStatus = a})

instance NFData GetSearchSuggestionsResponse where
