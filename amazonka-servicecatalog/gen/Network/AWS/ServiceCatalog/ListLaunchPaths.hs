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
-- Module      : Network.AWS.ServiceCatalog.ListLaunchPaths
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the paths to the specified product. A path is how the user has access to a specified product, and is necessary when provisioning a product. A path also determines the constraints put on the product.
--
--
--
-- This operation returns paginated results.
module Network.AWS.ServiceCatalog.ListLaunchPaths
    (
    -- * Creating a Request
      listLaunchPaths
    , ListLaunchPaths
    -- * Request Lenses
    , llpAcceptLanguage
    , llpPageToken
    , llpPageSize
    , llpProductId

    -- * Destructuring the Response
    , listLaunchPathsResponse
    , ListLaunchPathsResponse
    -- * Response Lenses
    , llprsNextPageToken
    , llprsLaunchPathSummaries
    , llprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'listLaunchPaths' smart constructor.
data ListLaunchPaths = ListLaunchPaths'
  { _llpAcceptLanguage :: !(Maybe Text)
  , _llpPageToken      :: !(Maybe Text)
  , _llpPageSize       :: !(Maybe Nat)
  , _llpProductId      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListLaunchPaths' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'llpAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'llpPageToken' - The page token for the next set of results. To retrieve the first set of results, use null.
--
-- * 'llpPageSize' - The maximum number of items to return with this call.
--
-- * 'llpProductId' - The product identifier.
listLaunchPaths
    :: Text -- ^ 'llpProductId'
    -> ListLaunchPaths
listLaunchPaths pProductId_ =
  ListLaunchPaths'
    { _llpAcceptLanguage = Nothing
    , _llpPageToken = Nothing
    , _llpPageSize = Nothing
    , _llpProductId = pProductId_
    }


-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
llpAcceptLanguage :: Lens' ListLaunchPaths (Maybe Text)
llpAcceptLanguage = lens _llpAcceptLanguage (\ s a -> s{_llpAcceptLanguage = a})

-- | The page token for the next set of results. To retrieve the first set of results, use null.
llpPageToken :: Lens' ListLaunchPaths (Maybe Text)
llpPageToken = lens _llpPageToken (\ s a -> s{_llpPageToken = a})

-- | The maximum number of items to return with this call.
llpPageSize :: Lens' ListLaunchPaths (Maybe Natural)
llpPageSize = lens _llpPageSize (\ s a -> s{_llpPageSize = a}) . mapping _Nat

-- | The product identifier.
llpProductId :: Lens' ListLaunchPaths Text
llpProductId = lens _llpProductId (\ s a -> s{_llpProductId = a})

instance AWSPager ListLaunchPaths where
        page rq rs
          | stop (rs ^. llprsNextPageToken) = Nothing
          | stop (rs ^. llprsLaunchPathSummaries) = Nothing
          | otherwise =
            Just $ rq & llpPageToken .~ rs ^. llprsNextPageToken

instance AWSRequest ListLaunchPaths where
        type Rs ListLaunchPaths = ListLaunchPathsResponse
        request = postJSON serviceCatalog
        response
          = receiveJSON
              (\ s h x ->
                 ListLaunchPathsResponse' <$>
                   (x .?> "NextPageToken") <*>
                     (x .?> "LaunchPathSummaries" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListLaunchPaths where

instance NFData ListLaunchPaths where

instance ToHeaders ListLaunchPaths where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.ListLaunchPaths" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListLaunchPaths where
        toJSON ListLaunchPaths'{..}
          = object
              (catMaybes
                 [("AcceptLanguage" .=) <$> _llpAcceptLanguage,
                  ("PageToken" .=) <$> _llpPageToken,
                  ("PageSize" .=) <$> _llpPageSize,
                  Just ("ProductId" .= _llpProductId)])

instance ToPath ListLaunchPaths where
        toPath = const "/"

instance ToQuery ListLaunchPaths where
        toQuery = const mempty

-- | /See:/ 'listLaunchPathsResponse' smart constructor.
data ListLaunchPathsResponse = ListLaunchPathsResponse'
  { _llprsNextPageToken       :: !(Maybe Text)
  , _llprsLaunchPathSummaries :: !(Maybe [LaunchPathSummary])
  , _llprsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListLaunchPathsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'llprsNextPageToken' - The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- * 'llprsLaunchPathSummaries' - Information about the launch path.
--
-- * 'llprsResponseStatus' - -- | The response status code.
listLaunchPathsResponse
    :: Int -- ^ 'llprsResponseStatus'
    -> ListLaunchPathsResponse
listLaunchPathsResponse pResponseStatus_ =
  ListLaunchPathsResponse'
    { _llprsNextPageToken = Nothing
    , _llprsLaunchPathSummaries = Nothing
    , _llprsResponseStatus = pResponseStatus_
    }


-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
llprsNextPageToken :: Lens' ListLaunchPathsResponse (Maybe Text)
llprsNextPageToken = lens _llprsNextPageToken (\ s a -> s{_llprsNextPageToken = a})

-- | Information about the launch path.
llprsLaunchPathSummaries :: Lens' ListLaunchPathsResponse [LaunchPathSummary]
llprsLaunchPathSummaries = lens _llprsLaunchPathSummaries (\ s a -> s{_llprsLaunchPathSummaries = a}) . _Default . _Coerce

-- | -- | The response status code.
llprsResponseStatus :: Lens' ListLaunchPathsResponse Int
llprsResponseStatus = lens _llprsResponseStatus (\ s a -> s{_llprsResponseStatus = a})

instance NFData ListLaunchPathsResponse where
