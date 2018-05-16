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
-- Module      : Network.AWS.IoT.ListThings
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists your things. Use the __attributeName__ and __attributeValue__ parameters to filter your things. For example, calling @ListThings@ with attributeName=Color and attributeValue=Red retrieves all things in the registry that contain an attribute __Color__ with the value __Red__ .
--
--
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListThings
    (
    -- * Creating a Request
      listThings
    , ListThings
    -- * Request Lenses
    , ltAttributeValue
    , ltThingTypeName
    , ltNextToken
    , ltAttributeName
    , ltMaxResults

    -- * Destructuring the Response
    , listThingsResponse
    , ListThingsResponse
    -- * Response Lenses
    , ltrsNextToken
    , ltrsThings
    , ltrsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the ListThings operation.
--
--
--
-- /See:/ 'listThings' smart constructor.
data ListThings = ListThings'
  { _ltAttributeValue :: !(Maybe Text)
  , _ltThingTypeName  :: !(Maybe Text)
  , _ltNextToken      :: !(Maybe Text)
  , _ltAttributeName  :: !(Maybe Text)
  , _ltMaxResults     :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListThings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltAttributeValue' - The attribute value used to search for things.
--
-- * 'ltThingTypeName' - The name of the thing type used to search for things.
--
-- * 'ltNextToken' - The token used to get the next set of results, or __null__ if there are no additional results.
--
-- * 'ltAttributeName' - The attribute name used to search for things.
--
-- * 'ltMaxResults' - The maximum number of results to return in this operation.
listThings
    :: ListThings
listThings =
  ListThings'
    { _ltAttributeValue = Nothing
    , _ltThingTypeName = Nothing
    , _ltNextToken = Nothing
    , _ltAttributeName = Nothing
    , _ltMaxResults = Nothing
    }


-- | The attribute value used to search for things.
ltAttributeValue :: Lens' ListThings (Maybe Text)
ltAttributeValue = lens _ltAttributeValue (\ s a -> s{_ltAttributeValue = a})

-- | The name of the thing type used to search for things.
ltThingTypeName :: Lens' ListThings (Maybe Text)
ltThingTypeName = lens _ltThingTypeName (\ s a -> s{_ltThingTypeName = a})

-- | The token used to get the next set of results, or __null__ if there are no additional results.
ltNextToken :: Lens' ListThings (Maybe Text)
ltNextToken = lens _ltNextToken (\ s a -> s{_ltNextToken = a})

-- | The attribute name used to search for things.
ltAttributeName :: Lens' ListThings (Maybe Text)
ltAttributeName = lens _ltAttributeName (\ s a -> s{_ltAttributeName = a})

-- | The maximum number of results to return in this operation.
ltMaxResults :: Lens' ListThings (Maybe Natural)
ltMaxResults = lens _ltMaxResults (\ s a -> s{_ltMaxResults = a}) . mapping _Nat

instance AWSPager ListThings where
        page rq rs
          | stop (rs ^. ltrsNextToken) = Nothing
          | stop (rs ^. ltrsThings) = Nothing
          | otherwise =
            Just $ rq & ltNextToken .~ rs ^. ltrsNextToken

instance AWSRequest ListThings where
        type Rs ListThings = ListThingsResponse
        request = get ioT
        response
          = receiveJSON
              (\ s h x ->
                 ListThingsResponse' <$>
                   (x .?> "nextToken") <*> (x .?> "things" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListThings where

instance NFData ListThings where

instance ToHeaders ListThings where
        toHeaders = const mempty

instance ToPath ListThings where
        toPath = const "/things"

instance ToQuery ListThings where
        toQuery ListThings'{..}
          = mconcat
              ["attributeValue" =: _ltAttributeValue,
               "thingTypeName" =: _ltThingTypeName,
               "nextToken" =: _ltNextToken,
               "attributeName" =: _ltAttributeName,
               "maxResults" =: _ltMaxResults]

-- | The output from the ListThings operation.
--
--
--
-- /See:/ 'listThingsResponse' smart constructor.
data ListThingsResponse = ListThingsResponse'
  { _ltrsNextToken      :: !(Maybe Text)
  , _ltrsThings         :: !(Maybe [ThingAttribute])
  , _ltrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListThingsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltrsNextToken' - The token used to get the next set of results, or __null__ if there are no additional results.
--
-- * 'ltrsThings' - The things.
--
-- * 'ltrsResponseStatus' - -- | The response status code.
listThingsResponse
    :: Int -- ^ 'ltrsResponseStatus'
    -> ListThingsResponse
listThingsResponse pResponseStatus_ =
  ListThingsResponse'
    { _ltrsNextToken = Nothing
    , _ltrsThings = Nothing
    , _ltrsResponseStatus = pResponseStatus_
    }


-- | The token used to get the next set of results, or __null__ if there are no additional results.
ltrsNextToken :: Lens' ListThingsResponse (Maybe Text)
ltrsNextToken = lens _ltrsNextToken (\ s a -> s{_ltrsNextToken = a})

-- | The things.
ltrsThings :: Lens' ListThingsResponse [ThingAttribute]
ltrsThings = lens _ltrsThings (\ s a -> s{_ltrsThings = a}) . _Default . _Coerce

-- | -- | The response status code.
ltrsResponseStatus :: Lens' ListThingsResponse Int
ltrsResponseStatus = lens _ltrsResponseStatus (\ s a -> s{_ltrsResponseStatus = a})

instance NFData ListThingsResponse where
