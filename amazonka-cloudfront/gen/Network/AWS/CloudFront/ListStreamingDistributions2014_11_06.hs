{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudFront.ListStreamingDistributions2014_11_06
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

-- | List streaming distributions.
--
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/ListStreamingDistributions2014_11_06.html>
module Network.AWS.CloudFront.ListStreamingDistributions2014_11_06
    (
    -- * Request
      ListStreamingDistributions2014_11_06
    -- ** Request constructor
    , listStreamingDistributions2014_11_06
    -- ** Request lenses
    , lsdMaxItems
    , lsdMarker

    -- * Response
    , ListStreamingDistributions2014_11_06Response
    -- ** Response constructor
    , listStreamingDistributions2014_11_06Response
    -- ** Response lenses
    , lsdrStreamingDistributionList
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CloudFront.Types

-- | /See:/ 'listStreamingDistributions2014_11_06' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsdMaxItems'
--
-- * 'lsdMarker'
data ListStreamingDistributions2014_11_06 = ListStreamingDistributions2014_11_06'{_lsdMaxItems :: Maybe Text, _lsdMarker :: Maybe Text} deriving (Eq, Read, Show)

-- | 'ListStreamingDistributions2014_11_06' smart constructor.
listStreamingDistributions2014_11_06 :: ListStreamingDistributions2014_11_06
listStreamingDistributions2014_11_06 = ListStreamingDistributions2014_11_06'{_lsdMaxItems = Nothing, _lsdMarker = Nothing};

-- | The maximum number of streaming distributions you want in the response
-- body.
lsdMaxItems :: Lens' ListStreamingDistributions2014_11_06 (Maybe Text)
lsdMaxItems = lens _lsdMaxItems (\ s a -> s{_lsdMaxItems = a});

-- | Use this when paginating results to indicate where to begin in your list
-- of streaming distributions. The results include distributions in the
-- list that occur after the marker. To get the next page of results, set
-- the Marker to the value of the NextMarker from the current page\'s
-- response (which is also the ID of the last distribution on that page).
lsdMarker :: Lens' ListStreamingDistributions2014_11_06 (Maybe Text)
lsdMarker = lens _lsdMarker (\ s a -> s{_lsdMarker = a});

instance AWSRequest
         ListStreamingDistributions2014_11_06 where
        type Sv ListStreamingDistributions2014_11_06 =
             CloudFront
        type Rs ListStreamingDistributions2014_11_06 =
             ListStreamingDistributions2014_11_06Response
        request = get
        response
          = receiveXML
              (\ s h x ->
                 ListStreamingDistributions2014_11_06Response' <$>
                   x .@ "StreamingDistributionList")

instance ToHeaders
         ListStreamingDistributions2014_11_06 where
        toHeaders = const mempty

instance ToPath ListStreamingDistributions2014_11_06
         where
        toPath = const "/2014-11-06/streaming-distribution"

instance ToQuery ListStreamingDistributions2014_11_06
         where
        toQuery ListStreamingDistributions2014_11_06'{..}
          = mconcat
              ["MaxItems" =: _lsdMaxItems, "Marker" =: _lsdMarker]

-- | /See:/ 'listStreamingDistributions2014_11_06Response' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsdrStreamingDistributionList'
newtype ListStreamingDistributions2014_11_06Response = ListStreamingDistributions2014_11_06Response'{_lsdrStreamingDistributionList :: StreamingDistributionList} deriving (Eq, Read, Show)

-- | 'ListStreamingDistributions2014_11_06Response' smart constructor.
listStreamingDistributions2014_11_06Response :: StreamingDistributionList -> ListStreamingDistributions2014_11_06Response
listStreamingDistributions2014_11_06Response pStreamingDistributionList = ListStreamingDistributions2014_11_06Response'{_lsdrStreamingDistributionList = pStreamingDistributionList};

-- | The StreamingDistributionList type.
lsdrStreamingDistributionList :: Lens' ListStreamingDistributions2014_11_06Response StreamingDistributionList
lsdrStreamingDistributionList = lens _lsdrStreamingDistributionList (\ s a -> s{_lsdrStreamingDistributionList = a});
