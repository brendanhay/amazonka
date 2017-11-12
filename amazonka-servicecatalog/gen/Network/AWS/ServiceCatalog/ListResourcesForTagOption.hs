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
-- Module      : Network.AWS.ServiceCatalog.ListResourcesForTagOption
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists resources associated with a TagOption.
--
--
module Network.AWS.ServiceCatalog.ListResourcesForTagOption
    (
    -- * Creating a Request
      listResourcesForTagOption
    , ListResourcesForTagOption
    -- * Request Lenses
    , lrftoResourceType
    , lrftoPageToken
    , lrftoPageSize
    , lrftoTagOptionId

    -- * Destructuring the Response
    , listResourcesForTagOptionResponse
    , ListResourcesForTagOptionResponse
    -- * Response Lenses
    , lrftorsResourceDetails
    , lrftorsPageToken
    , lrftorsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'listResourcesForTagOption' smart constructor.
data ListResourcesForTagOption = ListResourcesForTagOption'
  { _lrftoResourceType :: !(Maybe Text)
  , _lrftoPageToken    :: !(Maybe Text)
  , _lrftoPageSize     :: !(Maybe Nat)
  , _lrftoTagOptionId  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListResourcesForTagOption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrftoResourceType' - Resource type.
--
-- * 'lrftoPageToken' - The page token of the first page retrieved. If null, this retrieves the first page of size @PageSize@ .
--
-- * 'lrftoPageSize' - The maximum number of items to return in the results. If more results exist than fit in the specified @PageSize@ , the value of @NextPageToken@ in the response is non-null.
--
-- * 'lrftoTagOptionId' - Identifier of the TagOption.
listResourcesForTagOption
    :: Text -- ^ 'lrftoTagOptionId'
    -> ListResourcesForTagOption
listResourcesForTagOption pTagOptionId_ =
  ListResourcesForTagOption'
  { _lrftoResourceType = Nothing
  , _lrftoPageToken = Nothing
  , _lrftoPageSize = Nothing
  , _lrftoTagOptionId = pTagOptionId_
  }


-- | Resource type.
lrftoResourceType :: Lens' ListResourcesForTagOption (Maybe Text)
lrftoResourceType = lens _lrftoResourceType (\ s a -> s{_lrftoResourceType = a});

-- | The page token of the first page retrieved. If null, this retrieves the first page of size @PageSize@ .
lrftoPageToken :: Lens' ListResourcesForTagOption (Maybe Text)
lrftoPageToken = lens _lrftoPageToken (\ s a -> s{_lrftoPageToken = a});

-- | The maximum number of items to return in the results. If more results exist than fit in the specified @PageSize@ , the value of @NextPageToken@ in the response is non-null.
lrftoPageSize :: Lens' ListResourcesForTagOption (Maybe Natural)
lrftoPageSize = lens _lrftoPageSize (\ s a -> s{_lrftoPageSize = a}) . mapping _Nat;

-- | Identifier of the TagOption.
lrftoTagOptionId :: Lens' ListResourcesForTagOption Text
lrftoTagOptionId = lens _lrftoTagOptionId (\ s a -> s{_lrftoTagOptionId = a});

instance AWSRequest ListResourcesForTagOption where
        type Rs ListResourcesForTagOption =
             ListResourcesForTagOptionResponse
        request = postJSON serviceCatalog
        response
          = receiveJSON
              (\ s h x ->
                 ListResourcesForTagOptionResponse' <$>
                   (x .?> "ResourceDetails" .!@ mempty) <*>
                     (x .?> "PageToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListResourcesForTagOption where

instance NFData ListResourcesForTagOption where

instance ToHeaders ListResourcesForTagOption where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.ListResourcesForTagOption"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListResourcesForTagOption where
        toJSON ListResourcesForTagOption'{..}
          = object
              (catMaybes
                 [("ResourceType" .=) <$> _lrftoResourceType,
                  ("PageToken" .=) <$> _lrftoPageToken,
                  ("PageSize" .=) <$> _lrftoPageSize,
                  Just ("TagOptionId" .= _lrftoTagOptionId)])

instance ToPath ListResourcesForTagOption where
        toPath = const "/"

instance ToQuery ListResourcesForTagOption where
        toQuery = const mempty

-- | /See:/ 'listResourcesForTagOptionResponse' smart constructor.
data ListResourcesForTagOptionResponse = ListResourcesForTagOptionResponse'
  { _lrftorsResourceDetails :: !(Maybe [ResourceDetail])
  , _lrftorsPageToken       :: !(Maybe Text)
  , _lrftorsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListResourcesForTagOptionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrftorsResourceDetails' - The resulting detailed resource information.
--
-- * 'lrftorsPageToken' - The page token of the first page retrieved. If null, this retrieves the first page of size @PageSize@ .
--
-- * 'lrftorsResponseStatus' - -- | The response status code.
listResourcesForTagOptionResponse
    :: Int -- ^ 'lrftorsResponseStatus'
    -> ListResourcesForTagOptionResponse
listResourcesForTagOptionResponse pResponseStatus_ =
  ListResourcesForTagOptionResponse'
  { _lrftorsResourceDetails = Nothing
  , _lrftorsPageToken = Nothing
  , _lrftorsResponseStatus = pResponseStatus_
  }


-- | The resulting detailed resource information.
lrftorsResourceDetails :: Lens' ListResourcesForTagOptionResponse [ResourceDetail]
lrftorsResourceDetails = lens _lrftorsResourceDetails (\ s a -> s{_lrftorsResourceDetails = a}) . _Default . _Coerce;

-- | The page token of the first page retrieved. If null, this retrieves the first page of size @PageSize@ .
lrftorsPageToken :: Lens' ListResourcesForTagOptionResponse (Maybe Text)
lrftorsPageToken = lens _lrftorsPageToken (\ s a -> s{_lrftorsPageToken = a});

-- | -- | The response status code.
lrftorsResponseStatus :: Lens' ListResourcesForTagOptionResponse Int
lrftorsResponseStatus = lens _lrftorsResponseStatus (\ s a -> s{_lrftorsResponseStatus = a});

instance NFData ListResourcesForTagOptionResponse
         where
