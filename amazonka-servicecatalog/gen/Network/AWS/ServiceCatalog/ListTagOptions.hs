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
-- Module      : Network.AWS.ServiceCatalog.ListTagOptions
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists detailed TagOptions information.
--
--
module Network.AWS.ServiceCatalog.ListTagOptions
    (
    -- * Creating a Request
      listTagOptions
    , ListTagOptions
    -- * Request Lenses
    , ltoFilters
    , ltoPageToken
    , ltoPageSize

    -- * Destructuring the Response
    , listTagOptionsResponse
    , ListTagOptionsResponse
    -- * Response Lenses
    , ltorsPageToken
    , ltorsTagOptionDetails
    , ltorsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'listTagOptions' smart constructor.
data ListTagOptions = ListTagOptions'
  { _ltoFilters   :: !(Maybe ListTagOptionsFilters)
  , _ltoPageToken :: !(Maybe Text)
  , _ltoPageSize  :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTagOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltoFilters' - The list of filters with which to limit search results. If no search filters are specified, the output is all TagOptions.
--
-- * 'ltoPageToken' - The page token of the first page retrieved. If null, this retrieves the first page of size @PageSize@ .
--
-- * 'ltoPageSize' - The maximum number of items to return in the results. If more results exist than fit in the specified @PageSize@ , the value of @NextPageToken@ in the response is non-null.
listTagOptions
    :: ListTagOptions
listTagOptions =
  ListTagOptions'
  {_ltoFilters = Nothing, _ltoPageToken = Nothing, _ltoPageSize = Nothing}


-- | The list of filters with which to limit search results. If no search filters are specified, the output is all TagOptions.
ltoFilters :: Lens' ListTagOptions (Maybe ListTagOptionsFilters)
ltoFilters = lens _ltoFilters (\ s a -> s{_ltoFilters = a});

-- | The page token of the first page retrieved. If null, this retrieves the first page of size @PageSize@ .
ltoPageToken :: Lens' ListTagOptions (Maybe Text)
ltoPageToken = lens _ltoPageToken (\ s a -> s{_ltoPageToken = a});

-- | The maximum number of items to return in the results. If more results exist than fit in the specified @PageSize@ , the value of @NextPageToken@ in the response is non-null.
ltoPageSize :: Lens' ListTagOptions (Maybe Natural)
ltoPageSize = lens _ltoPageSize (\ s a -> s{_ltoPageSize = a}) . mapping _Nat;

instance AWSRequest ListTagOptions where
        type Rs ListTagOptions = ListTagOptionsResponse
        request = postJSON serviceCatalog
        response
          = receiveJSON
              (\ s h x ->
                 ListTagOptionsResponse' <$>
                   (x .?> "PageToken") <*>
                     (x .?> "TagOptionDetails" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListTagOptions where

instance NFData ListTagOptions where

instance ToHeaders ListTagOptions where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.ListTagOptions" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListTagOptions where
        toJSON ListTagOptions'{..}
          = object
              (catMaybes
                 [("Filters" .=) <$> _ltoFilters,
                  ("PageToken" .=) <$> _ltoPageToken,
                  ("PageSize" .=) <$> _ltoPageSize])

instance ToPath ListTagOptions where
        toPath = const "/"

instance ToQuery ListTagOptions where
        toQuery = const mempty

-- | /See:/ 'listTagOptionsResponse' smart constructor.
data ListTagOptionsResponse = ListTagOptionsResponse'
  { _ltorsPageToken        :: !(Maybe Text)
  , _ltorsTagOptionDetails :: !(Maybe [TagOptionDetail])
  , _ltorsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTagOptionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltorsPageToken' - The page token of the first page retrieved. If null, this retrieves the first page of size @PageSize@ .
--
-- * 'ltorsTagOptionDetails' - The resulting detailed TagOption information.
--
-- * 'ltorsResponseStatus' - -- | The response status code.
listTagOptionsResponse
    :: Int -- ^ 'ltorsResponseStatus'
    -> ListTagOptionsResponse
listTagOptionsResponse pResponseStatus_ =
  ListTagOptionsResponse'
  { _ltorsPageToken = Nothing
  , _ltorsTagOptionDetails = Nothing
  , _ltorsResponseStatus = pResponseStatus_
  }


-- | The page token of the first page retrieved. If null, this retrieves the first page of size @PageSize@ .
ltorsPageToken :: Lens' ListTagOptionsResponse (Maybe Text)
ltorsPageToken = lens _ltorsPageToken (\ s a -> s{_ltorsPageToken = a});

-- | The resulting detailed TagOption information.
ltorsTagOptionDetails :: Lens' ListTagOptionsResponse [TagOptionDetail]
ltorsTagOptionDetails = lens _ltorsTagOptionDetails (\ s a -> s{_ltorsTagOptionDetails = a}) . _Default . _Coerce;

-- | -- | The response status code.
ltorsResponseStatus :: Lens' ListTagOptionsResponse Int
ltorsResponseStatus = lens _ltorsResponseStatus (\ s a -> s{_ltorsResponseStatus = a});

instance NFData ListTagOptionsResponse where
