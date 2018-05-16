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
-- Module      : Network.AWS.IoT.ListThingTypes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the existing thing types.
--
--
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListThingTypes
    (
    -- * Creating a Request
      listThingTypes
    , ListThingTypes
    -- * Request Lenses
    , lttThingTypeName
    , lttNextToken
    , lttMaxResults

    -- * Destructuring the Response
    , listThingTypesResponse
    , ListThingTypesResponse
    -- * Response Lenses
    , lttrsThingTypes
    , lttrsNextToken
    , lttrsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the ListThingTypes operation.
--
--
--
-- /See:/ 'listThingTypes' smart constructor.
data ListThingTypes = ListThingTypes'
  { _lttThingTypeName :: !(Maybe Text)
  , _lttNextToken     :: !(Maybe Text)
  , _lttMaxResults    :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListThingTypes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lttThingTypeName' - The name of the thing type.
--
-- * 'lttNextToken' - The token for the next set of results, or __null__ if there are no additional results.
--
-- * 'lttMaxResults' - The maximum number of results to return in this operation.
listThingTypes
    :: ListThingTypes
listThingTypes =
  ListThingTypes'
    { _lttThingTypeName = Nothing
    , _lttNextToken = Nothing
    , _lttMaxResults = Nothing
    }


-- | The name of the thing type.
lttThingTypeName :: Lens' ListThingTypes (Maybe Text)
lttThingTypeName = lens _lttThingTypeName (\ s a -> s{_lttThingTypeName = a})

-- | The token for the next set of results, or __null__ if there are no additional results.
lttNextToken :: Lens' ListThingTypes (Maybe Text)
lttNextToken = lens _lttNextToken (\ s a -> s{_lttNextToken = a})

-- | The maximum number of results to return in this operation.
lttMaxResults :: Lens' ListThingTypes (Maybe Natural)
lttMaxResults = lens _lttMaxResults (\ s a -> s{_lttMaxResults = a}) . mapping _Nat

instance AWSPager ListThingTypes where
        page rq rs
          | stop (rs ^. lttrsNextToken) = Nothing
          | stop (rs ^. lttrsThingTypes) = Nothing
          | otherwise =
            Just $ rq & lttNextToken .~ rs ^. lttrsNextToken

instance AWSRequest ListThingTypes where
        type Rs ListThingTypes = ListThingTypesResponse
        request = get ioT
        response
          = receiveJSON
              (\ s h x ->
                 ListThingTypesResponse' <$>
                   (x .?> "thingTypes" .!@ mempty) <*>
                     (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListThingTypes where

instance NFData ListThingTypes where

instance ToHeaders ListThingTypes where
        toHeaders = const mempty

instance ToPath ListThingTypes where
        toPath = const "/thing-types"

instance ToQuery ListThingTypes where
        toQuery ListThingTypes'{..}
          = mconcat
              ["thingTypeName" =: _lttThingTypeName,
               "nextToken" =: _lttNextToken,
               "maxResults" =: _lttMaxResults]

-- | The output for the ListThingTypes operation.
--
--
--
-- /See:/ 'listThingTypesResponse' smart constructor.
data ListThingTypesResponse = ListThingTypesResponse'
  { _lttrsThingTypes     :: !(Maybe [ThingTypeDefinition])
  , _lttrsNextToken      :: !(Maybe Text)
  , _lttrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListThingTypesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lttrsThingTypes' - The thing types.
--
-- * 'lttrsNextToken' - The token for the next set of results, or __null__ if there are no additional results.
--
-- * 'lttrsResponseStatus' - -- | The response status code.
listThingTypesResponse
    :: Int -- ^ 'lttrsResponseStatus'
    -> ListThingTypesResponse
listThingTypesResponse pResponseStatus_ =
  ListThingTypesResponse'
    { _lttrsThingTypes = Nothing
    , _lttrsNextToken = Nothing
    , _lttrsResponseStatus = pResponseStatus_
    }


-- | The thing types.
lttrsThingTypes :: Lens' ListThingTypesResponse [ThingTypeDefinition]
lttrsThingTypes = lens _lttrsThingTypes (\ s a -> s{_lttrsThingTypes = a}) . _Default . _Coerce

-- | The token for the next set of results, or __null__ if there are no additional results.
lttrsNextToken :: Lens' ListThingTypesResponse (Maybe Text)
lttrsNextToken = lens _lttrsNextToken (\ s a -> s{_lttrsNextToken = a})

-- | -- | The response status code.
lttrsResponseStatus :: Lens' ListThingTypesResponse Int
lttrsResponseStatus = lens _lttrsResponseStatus (\ s a -> s{_lttrsResponseStatus = a})

instance NFData ListThingTypesResponse where
