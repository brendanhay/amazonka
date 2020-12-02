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
-- Module      : Network.AWS.CloudFront.ListDistributions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List distributions.
--
--
--
-- This operation returns paginated results.
module Network.AWS.CloudFront.ListDistributions
    (
    -- * Creating a Request
      listDistributions
    , ListDistributions
    -- * Request Lenses
    , ldMarker
    , ldMaxItems

    -- * Destructuring the Response
    , listDistributionsResponse
    , ListDistributionsResponse
    -- * Response Lenses
    , ldrsResponseStatus
    , ldrsDistributionList
    ) where

import Network.AWS.CloudFront.Types
import Network.AWS.CloudFront.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The request to list your distributions.
--
--
--
-- /See:/ 'listDistributions' smart constructor.
data ListDistributions = ListDistributions'
  { _ldMarker   :: !(Maybe Text)
  , _ldMaxItems :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListDistributions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldMarker' - Use this when paginating results to indicate where to begin in your list of distributions. The results include distributions in the list that occur after the marker. To get the next page of results, set the @Marker@ to the value of the @NextMarker@ from the current page's response (which is also the ID of the last distribution on that page).
--
-- * 'ldMaxItems' - The maximum number of distributions you want in the response body.
listDistributions
    :: ListDistributions
listDistributions =
  ListDistributions' {_ldMarker = Nothing, _ldMaxItems = Nothing}


-- | Use this when paginating results to indicate where to begin in your list of distributions. The results include distributions in the list that occur after the marker. To get the next page of results, set the @Marker@ to the value of the @NextMarker@ from the current page's response (which is also the ID of the last distribution on that page).
ldMarker :: Lens' ListDistributions (Maybe Text)
ldMarker = lens _ldMarker (\ s a -> s{_ldMarker = a})

-- | The maximum number of distributions you want in the response body.
ldMaxItems :: Lens' ListDistributions (Maybe Text)
ldMaxItems = lens _ldMaxItems (\ s a -> s{_ldMaxItems = a})

instance AWSPager ListDistributions where
        page rq rs
          | stop (rs ^. ldrsDistributionList . dlIsTruncated) =
            Nothing
          | isNothing
              (rs ^? ldrsDistributionList . dlNextMarker . _Just)
            = Nothing
          | otherwise =
            Just $ rq &
              ldMarker .~
                rs ^? ldrsDistributionList . dlNextMarker . _Just

instance AWSRequest ListDistributions where
        type Rs ListDistributions = ListDistributionsResponse
        request = get cloudFront
        response
          = receiveXML
              (\ s h x ->
                 ListDistributionsResponse' <$>
                   (pure (fromEnum s)) <*> (parseXML x))

instance Hashable ListDistributions where

instance NFData ListDistributions where

instance ToHeaders ListDistributions where
        toHeaders = const mempty

instance ToPath ListDistributions where
        toPath = const "/2017-10-30/distribution"

instance ToQuery ListDistributions where
        toQuery ListDistributions'{..}
          = mconcat
              ["Marker" =: _ldMarker, "MaxItems" =: _ldMaxItems]

-- | The returned result of the corresponding request.
--
--
--
-- /See:/ 'listDistributionsResponse' smart constructor.
data ListDistributionsResponse = ListDistributionsResponse'
  { _ldrsResponseStatus   :: !Int
  , _ldrsDistributionList :: !DistributionList
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListDistributionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldrsResponseStatus' - -- | The response status code.
--
-- * 'ldrsDistributionList' - The @DistributionList@ type.
listDistributionsResponse
    :: Int -- ^ 'ldrsResponseStatus'
    -> DistributionList -- ^ 'ldrsDistributionList'
    -> ListDistributionsResponse
listDistributionsResponse pResponseStatus_ pDistributionList_ =
  ListDistributionsResponse'
    { _ldrsResponseStatus = pResponseStatus_
    , _ldrsDistributionList = pDistributionList_
    }


-- | -- | The response status code.
ldrsResponseStatus :: Lens' ListDistributionsResponse Int
ldrsResponseStatus = lens _ldrsResponseStatus (\ s a -> s{_ldrsResponseStatus = a})

-- | The @DistributionList@ type.
ldrsDistributionList :: Lens' ListDistributionsResponse DistributionList
ldrsDistributionList = lens _ldrsDistributionList (\ s a -> s{_ldrsDistributionList = a})

instance NFData ListDistributionsResponse where
