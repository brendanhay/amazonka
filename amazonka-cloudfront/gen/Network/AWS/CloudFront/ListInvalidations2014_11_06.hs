{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudFront.ListInvalidations2014_11_06
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

-- | List invalidation batches.
--
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/ListInvalidations2014_11_06.html>
module Network.AWS.CloudFront.ListInvalidations2014_11_06
    (
    -- * Request
      ListInvalidations2014_11_06
    -- ** Request constructor
    , listInvalidations2014_11_06
    -- ** Request lenses
    , liMaxItems
    , liMarker
    , liDistributionId

    -- * Response
    , ListInvalidations2014_11_06Response
    -- ** Response constructor
    , listInvalidations2014_11_06Response
    -- ** Response lenses
    , lirInvalidationList
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CloudFront.Types

-- | /See:/ 'listInvalidations2014_11_06' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'liMaxItems'
--
-- * 'liMarker'
--
-- * 'liDistributionId'
data ListInvalidations2014_11_06 = ListInvalidations2014_11_06'{_liMaxItems :: Maybe Text, _liMarker :: Maybe Text, _liDistributionId :: Text} deriving (Eq, Read, Show)

-- | 'ListInvalidations2014_11_06' smart constructor.
listInvalidations2014_11_06 :: Text -> ListInvalidations2014_11_06
listInvalidations2014_11_06 pDistributionId = ListInvalidations2014_11_06'{_liMaxItems = Nothing, _liMarker = Nothing, _liDistributionId = pDistributionId};

-- | The maximum number of invalidation batches you want in the response
-- body.
liMaxItems :: Lens' ListInvalidations2014_11_06 (Maybe Text)
liMaxItems = lens _liMaxItems (\ s a -> s{_liMaxItems = a});

-- | Use this parameter when paginating results to indicate where to begin in
-- your list of invalidation batches. Because the results are returned in
-- decreasing order from most recent to oldest, the most recent results are
-- on the first page, the second page will contain earlier results, and so
-- on. To get the next page of results, set the Marker to the value of the
-- NextMarker from the current page\'s response. This value is the same as
-- the ID of the last invalidation batch on that page.
liMarker :: Lens' ListInvalidations2014_11_06 (Maybe Text)
liMarker = lens _liMarker (\ s a -> s{_liMarker = a});

-- | The distribution\'s id.
liDistributionId :: Lens' ListInvalidations2014_11_06 Text
liDistributionId = lens _liDistributionId (\ s a -> s{_liDistributionId = a});

instance AWSRequest ListInvalidations2014_11_06 where
        type Sv ListInvalidations2014_11_06 = CloudFront
        type Rs ListInvalidations2014_11_06 =
             ListInvalidations2014_11_06Response
        request = get
        response
          = receiveXML
              (\ s h x ->
                 ListInvalidations2014_11_06Response' <$>
                   x .@ "InvalidationList")

instance ToHeaders ListInvalidations2014_11_06 where
        toHeaders = const mempty

instance ToPath ListInvalidations2014_11_06 where
        toPath ListInvalidations2014_11_06'{..}
          = mconcat
              ["/2014-11-06/distribution/",
               toText _liDistributionId, "/invalidation"]

instance ToQuery ListInvalidations2014_11_06 where
        toQuery ListInvalidations2014_11_06'{..}
          = mconcat
              ["MaxItems" =: _liMaxItems, "Marker" =: _liMarker]

-- | /See:/ 'listInvalidations2014_11_06Response' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lirInvalidationList'
newtype ListInvalidations2014_11_06Response = ListInvalidations2014_11_06Response'{_lirInvalidationList :: InvalidationList} deriving (Eq, Read, Show)

-- | 'ListInvalidations2014_11_06Response' smart constructor.
listInvalidations2014_11_06Response :: InvalidationList -> ListInvalidations2014_11_06Response
listInvalidations2014_11_06Response pInvalidationList = ListInvalidations2014_11_06Response'{_lirInvalidationList = pInvalidationList};

-- | Information about invalidation batches.
lirInvalidationList :: Lens' ListInvalidations2014_11_06Response InvalidationList
lirInvalidationList = lens _lirInvalidationList (\ s a -> s{_lirInvalidationList = a});
