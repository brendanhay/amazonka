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
-- Module      : Network.AWS.IoT.ListAuthorizers
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the authorizers registered in your account.
--
--
module Network.AWS.IoT.ListAuthorizers
    (
    -- * Creating a Request
      listAuthorizers
    , ListAuthorizers
    -- * Request Lenses
    , laStatus
    , laMarker
    , laAscendingOrder
    , laPageSize

    -- * Destructuring the Response
    , listAuthorizersResponse
    , ListAuthorizersResponse
    -- * Response Lenses
    , larsAuthorizers
    , larsNextMarker
    , larsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listAuthorizers' smart constructor.
data ListAuthorizers = ListAuthorizers'
  { _laStatus         :: !(Maybe AuthorizerStatus)
  , _laMarker         :: !(Maybe Text)
  , _laAscendingOrder :: !(Maybe Bool)
  , _laPageSize       :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListAuthorizers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'laStatus' - The status of the list authorizers request.
--
-- * 'laMarker' - A marker used to get the next set of results.
--
-- * 'laAscendingOrder' - Return the list of authorizers in ascending alphabetical order.
--
-- * 'laPageSize' - The maximum number of results to return at one time.
listAuthorizers
    :: ListAuthorizers
listAuthorizers =
  ListAuthorizers'
    { _laStatus = Nothing
    , _laMarker = Nothing
    , _laAscendingOrder = Nothing
    , _laPageSize = Nothing
    }


-- | The status of the list authorizers request.
laStatus :: Lens' ListAuthorizers (Maybe AuthorizerStatus)
laStatus = lens _laStatus (\ s a -> s{_laStatus = a})

-- | A marker used to get the next set of results.
laMarker :: Lens' ListAuthorizers (Maybe Text)
laMarker = lens _laMarker (\ s a -> s{_laMarker = a})

-- | Return the list of authorizers in ascending alphabetical order.
laAscendingOrder :: Lens' ListAuthorizers (Maybe Bool)
laAscendingOrder = lens _laAscendingOrder (\ s a -> s{_laAscendingOrder = a})

-- | The maximum number of results to return at one time.
laPageSize :: Lens' ListAuthorizers (Maybe Natural)
laPageSize = lens _laPageSize (\ s a -> s{_laPageSize = a}) . mapping _Nat

instance AWSRequest ListAuthorizers where
        type Rs ListAuthorizers = ListAuthorizersResponse
        request = get ioT
        response
          = receiveJSON
              (\ s h x ->
                 ListAuthorizersResponse' <$>
                   (x .?> "authorizers" .!@ mempty) <*>
                     (x .?> "nextMarker")
                     <*> (pure (fromEnum s)))

instance Hashable ListAuthorizers where

instance NFData ListAuthorizers where

instance ToHeaders ListAuthorizers where
        toHeaders = const mempty

instance ToPath ListAuthorizers where
        toPath = const "/authorizers/"

instance ToQuery ListAuthorizers where
        toQuery ListAuthorizers'{..}
          = mconcat
              ["status" =: _laStatus, "marker" =: _laMarker,
               "isAscendingOrder" =: _laAscendingOrder,
               "pageSize" =: _laPageSize]

-- | /See:/ 'listAuthorizersResponse' smart constructor.
data ListAuthorizersResponse = ListAuthorizersResponse'
  { _larsAuthorizers    :: !(Maybe [AuthorizerSummary])
  , _larsNextMarker     :: !(Maybe Text)
  , _larsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListAuthorizersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'larsAuthorizers' - The authorizers.
--
-- * 'larsNextMarker' - A marker used to get the next set of results.
--
-- * 'larsResponseStatus' - -- | The response status code.
listAuthorizersResponse
    :: Int -- ^ 'larsResponseStatus'
    -> ListAuthorizersResponse
listAuthorizersResponse pResponseStatus_ =
  ListAuthorizersResponse'
    { _larsAuthorizers = Nothing
    , _larsNextMarker = Nothing
    , _larsResponseStatus = pResponseStatus_
    }


-- | The authorizers.
larsAuthorizers :: Lens' ListAuthorizersResponse [AuthorizerSummary]
larsAuthorizers = lens _larsAuthorizers (\ s a -> s{_larsAuthorizers = a}) . _Default . _Coerce

-- | A marker used to get the next set of results.
larsNextMarker :: Lens' ListAuthorizersResponse (Maybe Text)
larsNextMarker = lens _larsNextMarker (\ s a -> s{_larsNextMarker = a})

-- | -- | The response status code.
larsResponseStatus :: Lens' ListAuthorizersResponse Int
larsResponseStatus = lens _larsResponseStatus (\ s a -> s{_larsResponseStatus = a})

instance NFData ListAuthorizersResponse where
