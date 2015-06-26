{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudFront.ListDistributions
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | List distributions.
--
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/ListDistributions.html>
module Network.AWS.CloudFront.ListDistributions
    (
    -- * Request
      ListDistributions
    -- ** Request constructor
    , listDistributions
    -- ** Request lenses
    , ldMaxItems
    , ldMarker

    -- * Response
    , ListDistributionsResponse
    -- ** Response constructor
    , listDistributionsResponse
    -- ** Response lenses
    , ldrDistributionList
    , ldrStatusCode
    ) where

import Network.AWS.CloudFront.Types
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The request to list your distributions.
--
-- /See:/ 'listDistributions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldMaxItems'
--
-- * 'ldMarker'
data ListDistributions = ListDistributions'{_ldMaxItems :: Maybe Text, _ldMarker :: Maybe Text} deriving (Eq, Read, Show)

-- | 'ListDistributions' smart constructor.
listDistributions :: ListDistributions
listDistributions = ListDistributions'{_ldMaxItems = Nothing, _ldMarker = Nothing};

-- | The maximum number of distributions you want in the response body.
ldMaxItems :: Lens' ListDistributions (Maybe Text)
ldMaxItems = lens _ldMaxItems (\ s a -> s{_ldMaxItems = a});

-- | Use this when paginating results to indicate where to begin in your list
-- of distributions. The results include distributions in the list that
-- occur after the marker. To get the next page of results, set the Marker
-- to the value of the NextMarker from the current page\'s response (which
-- is also the ID of the last distribution on that page).
ldMarker :: Lens' ListDistributions (Maybe Text)
ldMarker = lens _ldMarker (\ s a -> s{_ldMarker = a});

instance AWSPager ListDistributions where
        page rq rs
          | stop (rs ^. ldrDistributionList . dlIsTruncated) =
            Nothing
          | isNothing
              (rs ^? ldrDistributionList . dlNextMarker . _Just)
            = Nothing
          | otherwise =
            Just $ rq &
              ldMarker .~
                rs ^? ldrDistributionList . dlNextMarker . _Just

instance AWSRequest ListDistributions where
        type Sv ListDistributions = CloudFront
        type Rs ListDistributions = ListDistributionsResponse
        request = get
        response
          = receiveXML
              (\ s h x ->
                 ListDistributionsResponse' <$>
                   (x .@ "DistributionList") <*> (pure (fromEnum s)))

instance ToHeaders ListDistributions where
        toHeaders = const mempty

instance ToPath ListDistributions where
        toPath = const "/2014-11-06/distribution"

instance ToQuery ListDistributions where
        toQuery ListDistributions'{..}
          = mconcat
              ["MaxItems" =: _ldMaxItems, "Marker" =: _ldMarker]

-- | The returned result of the corresponding request.
--
-- /See:/ 'listDistributionsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldrDistributionList'
--
-- * 'ldrStatusCode'
data ListDistributionsResponse = ListDistributionsResponse'{_ldrDistributionList :: DistributionList, _ldrStatusCode :: Int} deriving (Eq, Read, Show)

-- | 'ListDistributionsResponse' smart constructor.
listDistributionsResponse :: DistributionList -> Int -> ListDistributionsResponse
listDistributionsResponse pDistributionList pStatusCode = ListDistributionsResponse'{_ldrDistributionList = pDistributionList, _ldrStatusCode = pStatusCode};

-- | The DistributionList type.
ldrDistributionList :: Lens' ListDistributionsResponse DistributionList
ldrDistributionList = lens _ldrDistributionList (\ s a -> s{_ldrDistributionList = a});

-- | FIXME: Undocumented member.
ldrStatusCode :: Lens' ListDistributionsResponse Int
ldrStatusCode = lens _ldrStatusCode (\ s a -> s{_ldrStatusCode = a});
