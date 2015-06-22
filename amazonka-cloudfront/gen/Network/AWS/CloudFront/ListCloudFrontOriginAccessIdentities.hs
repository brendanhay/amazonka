{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudFront.ListCloudFrontOriginAccessIdentities
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

-- | List origin access identities.
--
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/ListCloudFrontOriginAccessIdentities.html>
module Network.AWS.CloudFront.ListCloudFrontOriginAccessIdentities
    (
    -- * Request
      ListCloudFrontOriginAccessIdentities
    -- ** Request constructor
    , listCloudFrontOriginAccessIdentities
    -- ** Request lenses
    , lcfoaiMaxItems
    , lcfoaiMarker

    -- * Response
    , ListCloudFrontOriginAccessIdentitiesResponse
    -- ** Response constructor
    , listCloudFrontOriginAccessIdentitiesResponse
    -- ** Response lenses
    , lcfoairCloudFrontOriginAccessIdentityList
    ) where

import Network.AWS.CloudFront.Types
import Network.AWS.Pagers
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listCloudFrontOriginAccessIdentities' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lcfoaiMaxItems'
--
-- * 'lcfoaiMarker'
data ListCloudFrontOriginAccessIdentities = ListCloudFrontOriginAccessIdentities'{_lcfoaiMaxItems :: Maybe Text, _lcfoaiMarker :: Maybe Text} deriving (Eq, Read, Show)

-- | 'ListCloudFrontOriginAccessIdentities' smart constructor.
listCloudFrontOriginAccessIdentities :: ListCloudFrontOriginAccessIdentities
listCloudFrontOriginAccessIdentities = ListCloudFrontOriginAccessIdentities'{_lcfoaiMaxItems = Nothing, _lcfoaiMarker = Nothing};

-- | The maximum number of origin access identities you want in the response
-- body.
lcfoaiMaxItems :: Lens' ListCloudFrontOriginAccessIdentities (Maybe Text)
lcfoaiMaxItems = lens _lcfoaiMaxItems (\ s a -> s{_lcfoaiMaxItems = a});

-- | Use this when paginating results to indicate where to begin in your list
-- of origin access identities. The results include identities in the list
-- that occur after the marker. To get the next page of results, set the
-- Marker to the value of the NextMarker from the current page\'s response
-- (which is also the ID of the last identity on that page).
lcfoaiMarker :: Lens' ListCloudFrontOriginAccessIdentities (Maybe Text)
lcfoaiMarker = lens _lcfoaiMarker (\ s a -> s{_lcfoaiMarker = a});

instance AWSPager
         ListCloudFrontOriginAccessIdentities where
        page rq rs
          | stop
              (rs ^.
                 lcfoairCloudFrontOriginAccessIdentityList .
                   cfoailIsTruncated)
            = Nothing
          | isNothing
              (rs ^?
                 lcfoairCloudFrontOriginAccessIdentityList .
                   cfoailNextMarker . _Just)
            = Nothing
          | otherwise =
            Just $ rq &
              lcfoaiMarker .~
                rs ^?
                  lcfoairCloudFrontOriginAccessIdentityList .
                    cfoailNextMarker . _Just

instance AWSRequest
         ListCloudFrontOriginAccessIdentities where
        type Sv ListCloudFrontOriginAccessIdentities =
             CloudFront
        type Rs ListCloudFrontOriginAccessIdentities =
             ListCloudFrontOriginAccessIdentitiesResponse
        request = get
        response
          = receiveXML
              (\ s h x ->
                 ListCloudFrontOriginAccessIdentitiesResponse' <$>
                   (x .@ "CloudFrontOriginAccessIdentityList"))

instance ToHeaders
         ListCloudFrontOriginAccessIdentities where
        toHeaders = const mempty

instance ToPath ListCloudFrontOriginAccessIdentities
         where
        toPath
          = const
              "/2014-11-06/origin-access-identity/cloudfront"

instance ToQuery ListCloudFrontOriginAccessIdentities
         where
        toQuery ListCloudFrontOriginAccessIdentities'{..}
          = mconcat
              ["MaxItems" =: _lcfoaiMaxItems,
               "Marker" =: _lcfoaiMarker]

-- | /See:/ 'listCloudFrontOriginAccessIdentitiesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lcfoairCloudFrontOriginAccessIdentityList'
newtype ListCloudFrontOriginAccessIdentitiesResponse = ListCloudFrontOriginAccessIdentitiesResponse'{_lcfoairCloudFrontOriginAccessIdentityList :: CloudFrontOriginAccessIdentityList} deriving (Eq, Read, Show)

-- | 'ListCloudFrontOriginAccessIdentitiesResponse' smart constructor.
listCloudFrontOriginAccessIdentitiesResponse :: CloudFrontOriginAccessIdentityList -> ListCloudFrontOriginAccessIdentitiesResponse
listCloudFrontOriginAccessIdentitiesResponse pCloudFrontOriginAccessIdentityList = ListCloudFrontOriginAccessIdentitiesResponse'{_lcfoairCloudFrontOriginAccessIdentityList = pCloudFrontOriginAccessIdentityList};

-- | The CloudFrontOriginAccessIdentityList type.
lcfoairCloudFrontOriginAccessIdentityList :: Lens' ListCloudFrontOriginAccessIdentitiesResponse CloudFrontOriginAccessIdentityList
lcfoairCloudFrontOriginAccessIdentityList = lens _lcfoairCloudFrontOriginAccessIdentityList (\ s a -> s{_lcfoairCloudFrontOriginAccessIdentityList = a});
