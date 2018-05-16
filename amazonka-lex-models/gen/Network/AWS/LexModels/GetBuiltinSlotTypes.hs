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
-- Module      : Network.AWS.LexModels.GetBuiltinSlotTypes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of built-in slot types that meet the specified criteria.
--
--
-- For a list of built-in slot types, see <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/slot-type-reference Slot Type Reference> in the /Alexa Skills Kit/ .
--
-- This operation requires permission for the @lex:GetBuiltInSlotTypes@ action.
--
--
-- This operation returns paginated results.
module Network.AWS.LexModels.GetBuiltinSlotTypes
    (
    -- * Creating a Request
      getBuiltinSlotTypes
    , GetBuiltinSlotTypes
    -- * Request Lenses
    , gbstLocale
    , gbstNextToken
    , gbstSignatureContains
    , gbstMaxResults

    -- * Destructuring the Response
    , getBuiltinSlotTypesResponse
    , GetBuiltinSlotTypesResponse
    -- * Response Lenses
    , gbstrsNextToken
    , gbstrsSlotTypes
    , gbstrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.LexModels.Types
import Network.AWS.LexModels.Types.Product
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getBuiltinSlotTypes' smart constructor.
data GetBuiltinSlotTypes = GetBuiltinSlotTypes'
  { _gbstLocale            :: !(Maybe Locale)
  , _gbstNextToken         :: !(Maybe Text)
  , _gbstSignatureContains :: !(Maybe Text)
  , _gbstMaxResults        :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBuiltinSlotTypes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbstLocale' - A list of locales that the slot type supports.
--
-- * 'gbstNextToken' - A pagination token that fetches the next page of slot types. If the response to this API call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of slot types, specify the pagination token in the next request.
--
-- * 'gbstSignatureContains' - Substring to match in built-in slot type signatures. A slot type will be returned if any part of its signature matches the substring. For example, "xyz" matches both "xyzabc" and "abcxyz."
--
-- * 'gbstMaxResults' - The maximum number of slot types to return in the response. The default is 10.
getBuiltinSlotTypes
    :: GetBuiltinSlotTypes
getBuiltinSlotTypes =
  GetBuiltinSlotTypes'
    { _gbstLocale = Nothing
    , _gbstNextToken = Nothing
    , _gbstSignatureContains = Nothing
    , _gbstMaxResults = Nothing
    }


-- | A list of locales that the slot type supports.
gbstLocale :: Lens' GetBuiltinSlotTypes (Maybe Locale)
gbstLocale = lens _gbstLocale (\ s a -> s{_gbstLocale = a})

-- | A pagination token that fetches the next page of slot types. If the response to this API call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of slot types, specify the pagination token in the next request.
gbstNextToken :: Lens' GetBuiltinSlotTypes (Maybe Text)
gbstNextToken = lens _gbstNextToken (\ s a -> s{_gbstNextToken = a})

-- | Substring to match in built-in slot type signatures. A slot type will be returned if any part of its signature matches the substring. For example, "xyz" matches both "xyzabc" and "abcxyz."
gbstSignatureContains :: Lens' GetBuiltinSlotTypes (Maybe Text)
gbstSignatureContains = lens _gbstSignatureContains (\ s a -> s{_gbstSignatureContains = a})

-- | The maximum number of slot types to return in the response. The default is 10.
gbstMaxResults :: Lens' GetBuiltinSlotTypes (Maybe Natural)
gbstMaxResults = lens _gbstMaxResults (\ s a -> s{_gbstMaxResults = a}) . mapping _Nat

instance AWSPager GetBuiltinSlotTypes where
        page rq rs
          | stop (rs ^. gbstrsNextToken) = Nothing
          | stop (rs ^. gbstrsSlotTypes) = Nothing
          | otherwise =
            Just $ rq & gbstNextToken .~ rs ^. gbstrsNextToken

instance AWSRequest GetBuiltinSlotTypes where
        type Rs GetBuiltinSlotTypes =
             GetBuiltinSlotTypesResponse
        request = get lexModels
        response
          = receiveJSON
              (\ s h x ->
                 GetBuiltinSlotTypesResponse' <$>
                   (x .?> "nextToken") <*>
                     (x .?> "slotTypes" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetBuiltinSlotTypes where

instance NFData GetBuiltinSlotTypes where

instance ToHeaders GetBuiltinSlotTypes where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetBuiltinSlotTypes where
        toPath = const "/builtins/slottypes/"

instance ToQuery GetBuiltinSlotTypes where
        toQuery GetBuiltinSlotTypes'{..}
          = mconcat
              ["locale" =: _gbstLocale,
               "nextToken" =: _gbstNextToken,
               "signatureContains" =: _gbstSignatureContains,
               "maxResults" =: _gbstMaxResults]

-- | /See:/ 'getBuiltinSlotTypesResponse' smart constructor.
data GetBuiltinSlotTypesResponse = GetBuiltinSlotTypesResponse'
  { _gbstrsNextToken      :: !(Maybe Text)
  , _gbstrsSlotTypes      :: !(Maybe [BuiltinSlotTypeMetadata])
  , _gbstrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBuiltinSlotTypesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbstrsNextToken' - If the response is truncated, the response includes a pagination token that you can use in your next request to fetch the next page of slot types.
--
-- * 'gbstrsSlotTypes' - An array of @BuiltInSlotTypeMetadata@ objects, one entry for each slot type returned.
--
-- * 'gbstrsResponseStatus' - -- | The response status code.
getBuiltinSlotTypesResponse
    :: Int -- ^ 'gbstrsResponseStatus'
    -> GetBuiltinSlotTypesResponse
getBuiltinSlotTypesResponse pResponseStatus_ =
  GetBuiltinSlotTypesResponse'
    { _gbstrsNextToken = Nothing
    , _gbstrsSlotTypes = Nothing
    , _gbstrsResponseStatus = pResponseStatus_
    }


-- | If the response is truncated, the response includes a pagination token that you can use in your next request to fetch the next page of slot types.
gbstrsNextToken :: Lens' GetBuiltinSlotTypesResponse (Maybe Text)
gbstrsNextToken = lens _gbstrsNextToken (\ s a -> s{_gbstrsNextToken = a})

-- | An array of @BuiltInSlotTypeMetadata@ objects, one entry for each slot type returned.
gbstrsSlotTypes :: Lens' GetBuiltinSlotTypesResponse [BuiltinSlotTypeMetadata]
gbstrsSlotTypes = lens _gbstrsSlotTypes (\ s a -> s{_gbstrsSlotTypes = a}) . _Default . _Coerce

-- | -- | The response status code.
gbstrsResponseStatus :: Lens' GetBuiltinSlotTypesResponse Int
gbstrsResponseStatus = lens _gbstrsResponseStatus (\ s a -> s{_gbstrsResponseStatus = a})

instance NFData GetBuiltinSlotTypesResponse where
