{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CloudFront.ListStreamingDistributions
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
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/ListStreamingDistributions.html>
module Network.AWS.CloudFront.ListStreamingDistributions
    (
    -- * Request
      ListStreamingDistributions
    -- ** Request constructor
    , listStreamingDistributions
    -- ** Request lenses
    , lsdMaxItems
    , lsdMarker

    -- * Response
    , ListStreamingDistributionsResponse
    -- ** Response constructor
    , listStreamingDistributionsResponse
    -- ** Response lenses
    , lsdrStreamingDistributionList
    , lsdrStatus
    ) where

import           Network.AWS.CloudFront.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The request to list your streaming distributions.
--
-- /See:/ 'listStreamingDistributions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsdMaxItems'
--
-- * 'lsdMarker'
data ListStreamingDistributions = ListStreamingDistributions'
    { _lsdMaxItems :: !(Maybe Text)
    , _lsdMarker   :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'ListStreamingDistributions' smart constructor.
listStreamingDistributions :: ListStreamingDistributions
listStreamingDistributions =
    ListStreamingDistributions'
    { _lsdMaxItems = Nothing
    , _lsdMarker = Nothing
    }

-- | The maximum number of streaming distributions you want in the response
-- body.
lsdMaxItems :: Lens' ListStreamingDistributions (Maybe Text)
lsdMaxItems = lens _lsdMaxItems (\ s a -> s{_lsdMaxItems = a});

-- | Use this when paginating results to indicate where to begin in your list
-- of streaming distributions. The results include distributions in the
-- list that occur after the marker. To get the next page of results, set
-- the Marker to the value of the NextMarker from the current page\'s
-- response (which is also the ID of the last distribution on that page).
lsdMarker :: Lens' ListStreamingDistributions (Maybe Text)
lsdMarker = lens _lsdMarker (\ s a -> s{_lsdMarker = a});

instance AWSRequest ListStreamingDistributions where
        type Sv ListStreamingDistributions = CloudFront
        type Rs ListStreamingDistributions =
             ListStreamingDistributionsResponse
        request = get
        response
          = receiveXML
              (\ s h x ->
                 ListStreamingDistributionsResponse' <$>
                   (x .@ "StreamingDistributionList") <*> (pure s))

instance ToHeaders ListStreamingDistributions where
        toHeaders = const mempty

instance ToPath ListStreamingDistributions where
        toPath = const "/2015-04-17/streaming-distribution"

instance ToQuery ListStreamingDistributions where
        toQuery ListStreamingDistributions'{..}
          = mconcat
              ["MaxItems" =: _lsdMaxItems, "Marker" =: _lsdMarker]

-- | The returned result of the corresponding request.
--
-- /See:/ 'listStreamingDistributionsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsdrStreamingDistributionList'
--
-- * 'lsdrStatus'
data ListStreamingDistributionsResponse = ListStreamingDistributionsResponse'
    { _lsdrStreamingDistributionList :: !StreamingDistributionList
    , _lsdrStatus                    :: !Status
    } deriving (Eq,Read,Show)

-- | 'ListStreamingDistributionsResponse' smart constructor.
listStreamingDistributionsResponse :: StreamingDistributionList -> Status -> ListStreamingDistributionsResponse
listStreamingDistributionsResponse pStreamingDistributionList pStatus =
    ListStreamingDistributionsResponse'
    { _lsdrStreamingDistributionList = pStreamingDistributionList
    , _lsdrStatus = pStatus
    }

-- | The StreamingDistributionList type.
lsdrStreamingDistributionList :: Lens' ListStreamingDistributionsResponse StreamingDistributionList
lsdrStreamingDistributionList = lens _lsdrStreamingDistributionList (\ s a -> s{_lsdrStreamingDistributionList = a});

-- | FIXME: Undocumented member.
lsdrStatus :: Lens' ListStreamingDistributionsResponse Status
lsdrStatus = lens _lsdrStatus (\ s a -> s{_lsdrStatus = a});
