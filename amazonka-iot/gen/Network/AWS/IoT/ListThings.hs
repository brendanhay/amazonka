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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists your things. You can pass an AttributeName or AttributeValue to filter your things (for example, \"ListThings where AttributeName=Color and AttributeValue=Red\").
module Network.AWS.IoT.ListThings
    (
    -- * Creating a Request
      listThings
    , ListThings
    -- * Request Lenses
    , ltAttributeValue
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

import           Network.AWS.IoT.Types
import           Network.AWS.IoT.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input for the ListThings operation.
--
-- /See:/ 'listThings' smart constructor.
data ListThings = ListThings'
    { _ltAttributeValue :: !(Maybe Text)
    , _ltNextToken      :: !(Maybe Text)
    , _ltAttributeName  :: !(Maybe Text)
    , _ltMaxResults     :: !(Maybe Nat)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListThings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltAttributeValue'
--
-- * 'ltNextToken'
--
-- * 'ltAttributeName'
--
-- * 'ltMaxResults'
listThings
    :: ListThings
listThings =
    ListThings'
    { _ltAttributeValue = Nothing
    , _ltNextToken = Nothing
    , _ltAttributeName = Nothing
    , _ltMaxResults = Nothing
    }

-- | The attribute value.
ltAttributeValue :: Lens' ListThings (Maybe Text)
ltAttributeValue = lens _ltAttributeValue (\ s a -> s{_ltAttributeValue = a});

-- | The token for the next value.
ltNextToken :: Lens' ListThings (Maybe Text)
ltNextToken = lens _ltNextToken (\ s a -> s{_ltNextToken = a});

-- | The attribute name.
ltAttributeName :: Lens' ListThings (Maybe Text)
ltAttributeName = lens _ltAttributeName (\ s a -> s{_ltAttributeName = a});

-- | The maximum number of results.
ltMaxResults :: Lens' ListThings (Maybe Natural)
ltMaxResults = lens _ltMaxResults (\ s a -> s{_ltMaxResults = a}) . mapping _Nat;

instance AWSRequest ListThings where
        type Rs ListThings = ListThingsResponse
        request = get ioT
        response
          = receiveJSON
              (\ s h x ->
                 ListThingsResponse' <$>
                   (x .?> "nextToken") <*> (x .?> "things" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListThings

instance NFData ListThings

instance ToHeaders ListThings where
        toHeaders = const mempty

instance ToPath ListThings where
        toPath = const "/things"

instance ToQuery ListThings where
        toQuery ListThings'{..}
          = mconcat
              ["attributeValue" =: _ltAttributeValue,
               "nextToken" =: _ltNextToken,
               "attributeName" =: _ltAttributeName,
               "maxResults" =: _ltMaxResults]

-- | The output from the ListThings operation.
--
-- /See:/ 'listThingsResponse' smart constructor.
data ListThingsResponse = ListThingsResponse'
    { _ltrsNextToken      :: !(Maybe Text)
    , _ltrsThings         :: !(Maybe [ThingAttribute])
    , _ltrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListThingsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltrsNextToken'
--
-- * 'ltrsThings'
--
-- * 'ltrsResponseStatus'
listThingsResponse
    :: Int -- ^ 'ltrsResponseStatus'
    -> ListThingsResponse
listThingsResponse pResponseStatus_ =
    ListThingsResponse'
    { _ltrsNextToken = Nothing
    , _ltrsThings = Nothing
    , _ltrsResponseStatus = pResponseStatus_
    }

-- | A token used to retrieve the next value.
ltrsNextToken :: Lens' ListThingsResponse (Maybe Text)
ltrsNextToken = lens _ltrsNextToken (\ s a -> s{_ltrsNextToken = a});

-- | The things.
ltrsThings :: Lens' ListThingsResponse [ThingAttribute]
ltrsThings = lens _ltrsThings (\ s a -> s{_ltrsThings = a}) . _Default . _Coerce;

-- | The response status code.
ltrsResponseStatus :: Lens' ListThingsResponse Int
ltrsResponseStatus = lens _ltrsResponseStatus (\ s a -> s{_ltrsResponseStatus = a});

instance NFData ListThingsResponse
