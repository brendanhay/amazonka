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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of all paths to a specified product. A path is how the user has access to a specified product, and is necessary when provisioning a product. A path also determines the constraints put on the product.
--
--
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

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.ServiceCatalog.Types
import           Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'listLaunchPaths' smart constructor.
data ListLaunchPaths = ListLaunchPaths'
    { _llpAcceptLanguage :: !(Maybe Text)
    , _llpPageToken      :: !(Maybe Text)
    , _llpPageSize       :: !(Maybe Nat)
    , _llpProductId      :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListLaunchPaths' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'llpAcceptLanguage' - The language code to use for this operation. Supported language codes are as follows: "en" (English) "jp" (Japanese) "zh" (Chinese) If no code is specified, "en" is used as the default.
--
-- * 'llpPageToken' - The page token of the first page retrieved. If null, this retrieves the first page of size @PageSize@ .
--
-- * 'llpPageSize' - The maximum number of items to return in the results. If more results exist than fit in the specified @PageSize@ , the value of @NextPageToken@ in the response is non-null.
--
-- * 'llpProductId' - The product identifier. Identifies the product for which to retrieve @LaunchPathSummaries@ information.
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

-- | The language code to use for this operation. Supported language codes are as follows: "en" (English) "jp" (Japanese) "zh" (Chinese) If no code is specified, "en" is used as the default.
llpAcceptLanguage :: Lens' ListLaunchPaths (Maybe Text)
llpAcceptLanguage = lens _llpAcceptLanguage (\ s a -> s{_llpAcceptLanguage = a});

-- | The page token of the first page retrieved. If null, this retrieves the first page of size @PageSize@ .
llpPageToken :: Lens' ListLaunchPaths (Maybe Text)
llpPageToken = lens _llpPageToken (\ s a -> s{_llpPageToken = a});

-- | The maximum number of items to return in the results. If more results exist than fit in the specified @PageSize@ , the value of @NextPageToken@ in the response is non-null.
llpPageSize :: Lens' ListLaunchPaths (Maybe Natural)
llpPageSize = lens _llpPageSize (\ s a -> s{_llpPageSize = a}) . mapping _Nat;

-- | The product identifier. Identifies the product for which to retrieve @LaunchPathSummaries@ information.
llpProductId :: Lens' ListLaunchPaths Text
llpProductId = lens _llpProductId (\ s a -> s{_llpProductId = a});

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

instance Hashable ListLaunchPaths

instance NFData ListLaunchPaths

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListLaunchPathsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'llprsNextPageToken' - The page token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
--
-- * 'llprsLaunchPathSummaries' - List of launch path information summaries for the specified @PageToken@ .
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

-- | The page token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
llprsNextPageToken :: Lens' ListLaunchPathsResponse (Maybe Text)
llprsNextPageToken = lens _llprsNextPageToken (\ s a -> s{_llprsNextPageToken = a});

-- | List of launch path information summaries for the specified @PageToken@ .
llprsLaunchPathSummaries :: Lens' ListLaunchPathsResponse [LaunchPathSummary]
llprsLaunchPathSummaries = lens _llprsLaunchPathSummaries (\ s a -> s{_llprsLaunchPathSummaries = a}) . _Default . _Coerce;

-- | -- | The response status code.
llprsResponseStatus :: Lens' ListLaunchPathsResponse Int
llprsResponseStatus = lens _llprsResponseStatus (\ s a -> s{_llprsResponseStatus = a});

instance NFData ListLaunchPathsResponse
