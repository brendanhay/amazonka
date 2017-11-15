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
-- Module      : Network.AWS.ServiceCatalog.ListRecordHistory
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of all performed requests, in the form of RecordDetails objects that are filtered as specified.
--
--
module Network.AWS.ServiceCatalog.ListRecordHistory
    (
    -- * Creating a Request
      listRecordHistory
    , ListRecordHistory
    -- * Request Lenses
    , lrhSearchFilter
    , lrhAcceptLanguage
    , lrhAccessLevelFilter
    , lrhPageToken
    , lrhPageSize

    -- * Destructuring the Response
    , listRecordHistoryResponse
    , ListRecordHistoryResponse
    -- * Response Lenses
    , lrhrsNextPageToken
    , lrhrsRecordDetails
    , lrhrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'listRecordHistory' smart constructor.
data ListRecordHistory = ListRecordHistory'
  { _lrhSearchFilter      :: !(Maybe ListRecordHistorySearchFilter)
  , _lrhAcceptLanguage    :: !(Maybe Text)
  , _lrhAccessLevelFilter :: !(Maybe AccessLevelFilter)
  , _lrhPageToken         :: !(Maybe Text)
  , _lrhPageSize          :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListRecordHistory' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrhSearchFilter' - The filter to limit search results.
--
-- * 'lrhAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'lrhAccessLevelFilter' - The access level for obtaining results. If left unspecified, @User@ level access is used.
--
-- * 'lrhPageToken' - The page token of the first page retrieved. If null, this retrieves the first page of size @PageSize@ .
--
-- * 'lrhPageSize' - The maximum number of items to return in the results. If more results exist than fit in the specified @PageSize@ , the value of @NextPageToken@ in the response is non-null.
listRecordHistory
    :: ListRecordHistory
listRecordHistory =
  ListRecordHistory'
  { _lrhSearchFilter = Nothing
  , _lrhAcceptLanguage = Nothing
  , _lrhAccessLevelFilter = Nothing
  , _lrhPageToken = Nothing
  , _lrhPageSize = Nothing
  }


-- | The filter to limit search results.
lrhSearchFilter :: Lens' ListRecordHistory (Maybe ListRecordHistorySearchFilter)
lrhSearchFilter = lens _lrhSearchFilter (\ s a -> s{_lrhSearchFilter = a});

-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
lrhAcceptLanguage :: Lens' ListRecordHistory (Maybe Text)
lrhAcceptLanguage = lens _lrhAcceptLanguage (\ s a -> s{_lrhAcceptLanguage = a});

-- | The access level for obtaining results. If left unspecified, @User@ level access is used.
lrhAccessLevelFilter :: Lens' ListRecordHistory (Maybe AccessLevelFilter)
lrhAccessLevelFilter = lens _lrhAccessLevelFilter (\ s a -> s{_lrhAccessLevelFilter = a});

-- | The page token of the first page retrieved. If null, this retrieves the first page of size @PageSize@ .
lrhPageToken :: Lens' ListRecordHistory (Maybe Text)
lrhPageToken = lens _lrhPageToken (\ s a -> s{_lrhPageToken = a});

-- | The maximum number of items to return in the results. If more results exist than fit in the specified @PageSize@ , the value of @NextPageToken@ in the response is non-null.
lrhPageSize :: Lens' ListRecordHistory (Maybe Natural)
lrhPageSize = lens _lrhPageSize (\ s a -> s{_lrhPageSize = a}) . mapping _Nat;

instance AWSRequest ListRecordHistory where
        type Rs ListRecordHistory = ListRecordHistoryResponse
        request = postJSON serviceCatalog
        response
          = receiveJSON
              (\ s h x ->
                 ListRecordHistoryResponse' <$>
                   (x .?> "NextPageToken") <*>
                     (x .?> "RecordDetails" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListRecordHistory where

instance NFData ListRecordHistory where

instance ToHeaders ListRecordHistory where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.ListRecordHistory" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListRecordHistory where
        toJSON ListRecordHistory'{..}
          = object
              (catMaybes
                 [("SearchFilter" .=) <$> _lrhSearchFilter,
                  ("AcceptLanguage" .=) <$> _lrhAcceptLanguage,
                  ("AccessLevelFilter" .=) <$> _lrhAccessLevelFilter,
                  ("PageToken" .=) <$> _lrhPageToken,
                  ("PageSize" .=) <$> _lrhPageSize])

instance ToPath ListRecordHistory where
        toPath = const "/"

instance ToQuery ListRecordHistory where
        toQuery = const mempty

-- | /See:/ 'listRecordHistoryResponse' smart constructor.
data ListRecordHistoryResponse = ListRecordHistoryResponse'
  { _lrhrsNextPageToken  :: !(Maybe Text)
  , _lrhrsRecordDetails  :: !(Maybe [RecordDetail])
  , _lrhrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListRecordHistoryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrhrsNextPageToken' - The page token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
--
-- * 'lrhrsRecordDetails' - A list of record detail objects, listed in reverse chronological order.
--
-- * 'lrhrsResponseStatus' - -- | The response status code.
listRecordHistoryResponse
    :: Int -- ^ 'lrhrsResponseStatus'
    -> ListRecordHistoryResponse
listRecordHistoryResponse pResponseStatus_ =
  ListRecordHistoryResponse'
  { _lrhrsNextPageToken = Nothing
  , _lrhrsRecordDetails = Nothing
  , _lrhrsResponseStatus = pResponseStatus_
  }


-- | The page token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
lrhrsNextPageToken :: Lens' ListRecordHistoryResponse (Maybe Text)
lrhrsNextPageToken = lens _lrhrsNextPageToken (\ s a -> s{_lrhrsNextPageToken = a});

-- | A list of record detail objects, listed in reverse chronological order.
lrhrsRecordDetails :: Lens' ListRecordHistoryResponse [RecordDetail]
lrhrsRecordDetails = lens _lrhrsRecordDetails (\ s a -> s{_lrhrsRecordDetails = a}) . _Default . _Coerce;

-- | -- | The response status code.
lrhrsResponseStatus :: Lens' ListRecordHistoryResponse Int
lrhrsResponseStatus = lens _lrhrsResponseStatus (\ s a -> s{_lrhrsResponseStatus = a});

instance NFData ListRecordHistoryResponse where
