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
-- Module      : Network.AWS.IoT.ListIndices
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the search indices.
--
--
module Network.AWS.IoT.ListIndices
    (
    -- * Creating a Request
      listIndices
    , ListIndices
    -- * Request Lenses
    , liNextToken
    , liMaxResults

    -- * Destructuring the Response
    , listIndicesResponse
    , ListIndicesResponse
    -- * Response Lenses
    , lirsNextToken
    , lirsIndexNames
    , lirsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listIndices' smart constructor.
data ListIndices = ListIndices'
  { _liNextToken  :: !(Maybe Text)
  , _liMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListIndices' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'liNextToken' - The token used to get the next set of results, or __null__ if there are no additional results.
--
-- * 'liMaxResults' - The maximum number of results to return at one time.
listIndices
    :: ListIndices
listIndices = ListIndices' {_liNextToken = Nothing, _liMaxResults = Nothing}


-- | The token used to get the next set of results, or __null__ if there are no additional results.
liNextToken :: Lens' ListIndices (Maybe Text)
liNextToken = lens _liNextToken (\ s a -> s{_liNextToken = a})

-- | The maximum number of results to return at one time.
liMaxResults :: Lens' ListIndices (Maybe Natural)
liMaxResults = lens _liMaxResults (\ s a -> s{_liMaxResults = a}) . mapping _Nat

instance AWSRequest ListIndices where
        type Rs ListIndices = ListIndicesResponse
        request = get ioT
        response
          = receiveJSON
              (\ s h x ->
                 ListIndicesResponse' <$>
                   (x .?> "nextToken") <*>
                     (x .?> "indexNames" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListIndices where

instance NFData ListIndices where

instance ToHeaders ListIndices where
        toHeaders = const mempty

instance ToPath ListIndices where
        toPath = const "/indices"

instance ToQuery ListIndices where
        toQuery ListIndices'{..}
          = mconcat
              ["nextToken" =: _liNextToken,
               "maxResults" =: _liMaxResults]

-- | /See:/ 'listIndicesResponse' smart constructor.
data ListIndicesResponse = ListIndicesResponse'
  { _lirsNextToken      :: !(Maybe Text)
  , _lirsIndexNames     :: !(Maybe [Text])
  , _lirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListIndicesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lirsNextToken' - The token used to get the next set of results, or __null__ if there are no additional results.
--
-- * 'lirsIndexNames' - The index names.
--
-- * 'lirsResponseStatus' - -- | The response status code.
listIndicesResponse
    :: Int -- ^ 'lirsResponseStatus'
    -> ListIndicesResponse
listIndicesResponse pResponseStatus_ =
  ListIndicesResponse'
    { _lirsNextToken = Nothing
    , _lirsIndexNames = Nothing
    , _lirsResponseStatus = pResponseStatus_
    }


-- | The token used to get the next set of results, or __null__ if there are no additional results.
lirsNextToken :: Lens' ListIndicesResponse (Maybe Text)
lirsNextToken = lens _lirsNextToken (\ s a -> s{_lirsNextToken = a})

-- | The index names.
lirsIndexNames :: Lens' ListIndicesResponse [Text]
lirsIndexNames = lens _lirsIndexNames (\ s a -> s{_lirsIndexNames = a}) . _Default . _Coerce

-- | -- | The response status code.
lirsResponseStatus :: Lens' ListIndicesResponse Int
lirsResponseStatus = lens _lirsResponseStatus (\ s a -> s{_lirsResponseStatus = a})

instance NFData ListIndicesResponse where
