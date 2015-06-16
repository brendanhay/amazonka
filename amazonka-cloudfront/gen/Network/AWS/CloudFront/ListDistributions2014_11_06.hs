{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudFront.ListDistributions2014_11_06
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
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/ListDistributions2014_11_06.html>
module Network.AWS.CloudFront.ListDistributions2014_11_06
    (
    -- * Request
      ListDistributions2014_11_06
    -- ** Request constructor
    , listDistributions2014_11_06
    -- ** Request lenses
    , ldMaxItems
    , ldMarker

    -- * Response
    , ListDistributions2014_11_06Response
    -- ** Response constructor
    , listDistributions2014_11_06Response
    -- ** Response lenses
    , ldrDistributionList
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CloudFront.Types

-- | /See:/ 'listDistributions2014_11_06' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldMaxItems'
--
-- * 'ldMarker'
data ListDistributions2014_11_06 = ListDistributions2014_11_06'{_ldMaxItems :: Maybe Text, _ldMarker :: Maybe Text} deriving (Eq, Read, Show)

-- | 'ListDistributions2014_11_06' smart constructor.
listDistributions2014_11_06 :: ListDistributions2014_11_06
listDistributions2014_11_06 = ListDistributions2014_11_06'{_ldMaxItems = Nothing, _ldMarker = Nothing};

-- | The maximum number of distributions you want in the response body.
ldMaxItems :: Lens' ListDistributions2014_11_06 (Maybe Text)
ldMaxItems = lens _ldMaxItems (\ s a -> s{_ldMaxItems = a});

-- | Use this when paginating results to indicate where to begin in your list
-- of distributions. The results include distributions in the list that
-- occur after the marker. To get the next page of results, set the Marker
-- to the value of the NextMarker from the current page\'s response (which
-- is also the ID of the last distribution on that page).
ldMarker :: Lens' ListDistributions2014_11_06 (Maybe Text)
ldMarker = lens _ldMarker (\ s a -> s{_ldMarker = a});

instance AWSRequest ListDistributions2014_11_06 where
        type Sv ListDistributions2014_11_06 = CloudFront
        type Rs ListDistributions2014_11_06 =
             ListDistributions2014_11_06Response
        request = get
        response
          = receiveXML
              (\ s h x ->
                 ListDistributions2014_11_06Response' <$>
                   (x .@ "DistributionList"))

instance ToHeaders ListDistributions2014_11_06 where
        toHeaders = const mempty

instance ToPath ListDistributions2014_11_06 where
        toPath = const "/2014-11-06/distribution"

instance ToQuery ListDistributions2014_11_06 where
        toQuery ListDistributions2014_11_06'{..}
          = mconcat
              ["MaxItems" =: _ldMaxItems, "Marker" =: _ldMarker]

-- | /See:/ 'listDistributions2014_11_06Response' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldrDistributionList'
newtype ListDistributions2014_11_06Response = ListDistributions2014_11_06Response'{_ldrDistributionList :: DistributionList} deriving (Eq, Read, Show)

-- | 'ListDistributions2014_11_06Response' smart constructor.
listDistributions2014_11_06Response :: DistributionList -> ListDistributions2014_11_06Response
listDistributions2014_11_06Response pDistributionList = ListDistributions2014_11_06Response'{_ldrDistributionList = pDistributionList};

-- | The DistributionList type.
ldrDistributionList :: Lens' ListDistributions2014_11_06Response DistributionList
ldrDistributionList = lens _ldrDistributionList (\ s a -> s{_ldrDistributionList = a});
