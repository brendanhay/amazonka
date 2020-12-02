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
-- Module      : Network.AWS.SSM.ListResourceDataSync
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists your resource data sync configurations. Includes information about the last time a sync attempted to start, the last sync status, and the last time a sync successfully completed.
--
--
-- The number of sync configurations might be too large to return using a single call to @ListResourceDataSync@ . You can limit the number of sync configurations returned by using the @MaxResults@ parameter. To determine whether there are more sync configurations to list, check the value of @NextToken@ in the output. If there are more sync configurations to list, you can request them by specifying the @NextToken@ returned in the call to the parameter of a subsequent call.
--
module Network.AWS.SSM.ListResourceDataSync
    (
    -- * Creating a Request
      listResourceDataSync
    , ListResourceDataSync
    -- * Request Lenses
    , lrdsNextToken
    , lrdsMaxResults

    -- * Destructuring the Response
    , listResourceDataSyncResponse
    , ListResourceDataSyncResponse
    -- * Response Lenses
    , lrdsrsResourceDataSyncItems
    , lrdsrsNextToken
    , lrdsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'listResourceDataSync' smart constructor.
data ListResourceDataSync = ListResourceDataSync'
  { _lrdsNextToken  :: !(Maybe Text)
  , _lrdsMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListResourceDataSync' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrdsNextToken' - A token to start the list. Use this token to get the next set of results.
--
-- * 'lrdsMaxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
listResourceDataSync
    :: ListResourceDataSync
listResourceDataSync =
  ListResourceDataSync' {_lrdsNextToken = Nothing, _lrdsMaxResults = Nothing}


-- | A token to start the list. Use this token to get the next set of results.
lrdsNextToken :: Lens' ListResourceDataSync (Maybe Text)
lrdsNextToken = lens _lrdsNextToken (\ s a -> s{_lrdsNextToken = a})

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
lrdsMaxResults :: Lens' ListResourceDataSync (Maybe Natural)
lrdsMaxResults = lens _lrdsMaxResults (\ s a -> s{_lrdsMaxResults = a}) . mapping _Nat

instance AWSRequest ListResourceDataSync where
        type Rs ListResourceDataSync =
             ListResourceDataSyncResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 ListResourceDataSyncResponse' <$>
                   (x .?> "ResourceDataSyncItems" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListResourceDataSync where

instance NFData ListResourceDataSync where

instance ToHeaders ListResourceDataSync where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.ListResourceDataSync" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListResourceDataSync where
        toJSON ListResourceDataSync'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lrdsNextToken,
                  ("MaxResults" .=) <$> _lrdsMaxResults])

instance ToPath ListResourceDataSync where
        toPath = const "/"

instance ToQuery ListResourceDataSync where
        toQuery = const mempty

-- | /See:/ 'listResourceDataSyncResponse' smart constructor.
data ListResourceDataSyncResponse = ListResourceDataSyncResponse'
  { _lrdsrsResourceDataSyncItems :: !(Maybe [ResourceDataSyncItem])
  , _lrdsrsNextToken             :: !(Maybe Text)
  , _lrdsrsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListResourceDataSyncResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrdsrsResourceDataSyncItems' - A list of your current Resource Data Sync configurations and their statuses.
--
-- * 'lrdsrsNextToken' - The token for the next set of items to return. Use this token to get the next set of results.
--
-- * 'lrdsrsResponseStatus' - -- | The response status code.
listResourceDataSyncResponse
    :: Int -- ^ 'lrdsrsResponseStatus'
    -> ListResourceDataSyncResponse
listResourceDataSyncResponse pResponseStatus_ =
  ListResourceDataSyncResponse'
    { _lrdsrsResourceDataSyncItems = Nothing
    , _lrdsrsNextToken = Nothing
    , _lrdsrsResponseStatus = pResponseStatus_
    }


-- | A list of your current Resource Data Sync configurations and their statuses.
lrdsrsResourceDataSyncItems :: Lens' ListResourceDataSyncResponse [ResourceDataSyncItem]
lrdsrsResourceDataSyncItems = lens _lrdsrsResourceDataSyncItems (\ s a -> s{_lrdsrsResourceDataSyncItems = a}) . _Default . _Coerce

-- | The token for the next set of items to return. Use this token to get the next set of results.
lrdsrsNextToken :: Lens' ListResourceDataSyncResponse (Maybe Text)
lrdsrsNextToken = lens _lrdsrsNextToken (\ s a -> s{_lrdsrsNextToken = a})

-- | -- | The response status code.
lrdsrsResponseStatus :: Lens' ListResourceDataSyncResponse Int
lrdsrsResponseStatus = lens _lrdsrsResponseStatus (\ s a -> s{_lrdsrsResponseStatus = a})

instance NFData ListResourceDataSyncResponse where
