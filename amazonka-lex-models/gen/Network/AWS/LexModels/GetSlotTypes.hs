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
-- Module      : Network.AWS.LexModels.GetSlotTypes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns slot type information as follows:
--
--
--     * If you specify the @nameContains@ field, returns the @> LATEST@ version of all slot types that contain the specified string.
--
--     * If you don't specify the @nameContains@ field, returns information about the @> LATEST@ version of all slot types.
--
--
--
-- The operation requires permission for the @lex:GetSlotTypes@ action.
--
--
-- This operation returns paginated results.
module Network.AWS.LexModels.GetSlotTypes
    (
    -- * Creating a Request
      getSlotTypes
    , GetSlotTypes
    -- * Request Lenses
    , gstNameContains
    , gstNextToken
    , gstMaxResults

    -- * Destructuring the Response
    , getSlotTypesResponse
    , GetSlotTypesResponse
    -- * Response Lenses
    , gstrsNextToken
    , gstrsSlotTypes
    , gstrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.LexModels.Types
import Network.AWS.LexModels.Types.Product
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getSlotTypes' smart constructor.
data GetSlotTypes = GetSlotTypes'
  { _gstNameContains :: !(Maybe Text)
  , _gstNextToken    :: !(Maybe Text)
  , _gstMaxResults   :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetSlotTypes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gstNameContains' - Substring to match in slot type names. A slot type will be returned if any part of its name matches the substring. For example, "xyz" matches both "xyzabc" and "abcxyz."
--
-- * 'gstNextToken' - A pagination token that fetches the next page of slot types. If the response to this API call is truncated, Amazon Lex returns a pagination token in the response. To fetch next page of slot types, specify the pagination token in the next request.
--
-- * 'gstMaxResults' - The maximum number of slot types to return in the response. The default is 10.
getSlotTypes
    :: GetSlotTypes
getSlotTypes =
  GetSlotTypes'
    { _gstNameContains = Nothing
    , _gstNextToken = Nothing
    , _gstMaxResults = Nothing
    }


-- | Substring to match in slot type names. A slot type will be returned if any part of its name matches the substring. For example, "xyz" matches both "xyzabc" and "abcxyz."
gstNameContains :: Lens' GetSlotTypes (Maybe Text)
gstNameContains = lens _gstNameContains (\ s a -> s{_gstNameContains = a})

-- | A pagination token that fetches the next page of slot types. If the response to this API call is truncated, Amazon Lex returns a pagination token in the response. To fetch next page of slot types, specify the pagination token in the next request.
gstNextToken :: Lens' GetSlotTypes (Maybe Text)
gstNextToken = lens _gstNextToken (\ s a -> s{_gstNextToken = a})

-- | The maximum number of slot types to return in the response. The default is 10.
gstMaxResults :: Lens' GetSlotTypes (Maybe Natural)
gstMaxResults = lens _gstMaxResults (\ s a -> s{_gstMaxResults = a}) . mapping _Nat

instance AWSPager GetSlotTypes where
        page rq rs
          | stop (rs ^. gstrsNextToken) = Nothing
          | stop (rs ^. gstrsSlotTypes) = Nothing
          | otherwise =
            Just $ rq & gstNextToken .~ rs ^. gstrsNextToken

instance AWSRequest GetSlotTypes where
        type Rs GetSlotTypes = GetSlotTypesResponse
        request = get lexModels
        response
          = receiveJSON
              (\ s h x ->
                 GetSlotTypesResponse' <$>
                   (x .?> "nextToken") <*>
                     (x .?> "slotTypes" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetSlotTypes where

instance NFData GetSlotTypes where

instance ToHeaders GetSlotTypes where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetSlotTypes where
        toPath = const "/slottypes/"

instance ToQuery GetSlotTypes where
        toQuery GetSlotTypes'{..}
          = mconcat
              ["nameContains" =: _gstNameContains,
               "nextToken" =: _gstNextToken,
               "maxResults" =: _gstMaxResults]

-- | /See:/ 'getSlotTypesResponse' smart constructor.
data GetSlotTypesResponse = GetSlotTypesResponse'
  { _gstrsNextToken      :: !(Maybe Text)
  , _gstrsSlotTypes      :: !(Maybe [SlotTypeMetadata])
  , _gstrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetSlotTypesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gstrsNextToken' - If the response is truncated, it includes a pagination token that you can specify in your next request to fetch the next page of slot types.
--
-- * 'gstrsSlotTypes' - An array of objects, one for each slot type, that provides information such as the name of the slot type, the version, and a description.
--
-- * 'gstrsResponseStatus' - -- | The response status code.
getSlotTypesResponse
    :: Int -- ^ 'gstrsResponseStatus'
    -> GetSlotTypesResponse
getSlotTypesResponse pResponseStatus_ =
  GetSlotTypesResponse'
    { _gstrsNextToken = Nothing
    , _gstrsSlotTypes = Nothing
    , _gstrsResponseStatus = pResponseStatus_
    }


-- | If the response is truncated, it includes a pagination token that you can specify in your next request to fetch the next page of slot types.
gstrsNextToken :: Lens' GetSlotTypesResponse (Maybe Text)
gstrsNextToken = lens _gstrsNextToken (\ s a -> s{_gstrsNextToken = a})

-- | An array of objects, one for each slot type, that provides information such as the name of the slot type, the version, and a description.
gstrsSlotTypes :: Lens' GetSlotTypesResponse [SlotTypeMetadata]
gstrsSlotTypes = lens _gstrsSlotTypes (\ s a -> s{_gstrsSlotTypes = a}) . _Default . _Coerce

-- | -- | The response status code.
gstrsResponseStatus :: Lens' GetSlotTypesResponse Int
gstrsResponseStatus = lens _gstrsResponseStatus (\ s a -> s{_gstrsResponseStatus = a})

instance NFData GetSlotTypesResponse where
