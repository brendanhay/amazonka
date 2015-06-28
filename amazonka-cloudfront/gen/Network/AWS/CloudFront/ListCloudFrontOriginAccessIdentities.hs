{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

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
    , lcfoairStatus
    ) where

import           Network.AWS.CloudFront.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The request to list origin access identities.
--
-- /See:/ 'listCloudFrontOriginAccessIdentities' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lcfoaiMaxItems'
--
-- * 'lcfoaiMarker'
data ListCloudFrontOriginAccessIdentities = ListCloudFrontOriginAccessIdentities'
    { _lcfoaiMaxItems :: !(Maybe Text)
    , _lcfoaiMarker   :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'ListCloudFrontOriginAccessIdentities' smart constructor.
listCloudFrontOriginAccessIdentities :: ListCloudFrontOriginAccessIdentities
listCloudFrontOriginAccessIdentities =
    ListCloudFrontOriginAccessIdentities'
    { _lcfoaiMaxItems = Nothing
    , _lcfoaiMarker = Nothing
    }

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
                   (x .@ "CloudFrontOriginAccessIdentityList") <*>
                     (pure s))

instance ToHeaders
         ListCloudFrontOriginAccessIdentities where
        toHeaders = const mempty

instance ToPath ListCloudFrontOriginAccessIdentities
         where
        toPath
          = const
              "/2015-04-17/origin-access-identity/cloudfront"

instance ToQuery ListCloudFrontOriginAccessIdentities
         where
        toQuery ListCloudFrontOriginAccessIdentities'{..}
          = mconcat
              ["MaxItems" =: _lcfoaiMaxItems,
               "Marker" =: _lcfoaiMarker]

-- | The returned result of the corresponding request.
--
-- /See:/ 'listCloudFrontOriginAccessIdentitiesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lcfoairCloudFrontOriginAccessIdentityList'
--
-- * 'lcfoairStatus'
data ListCloudFrontOriginAccessIdentitiesResponse = ListCloudFrontOriginAccessIdentitiesResponse'
    { _lcfoairCloudFrontOriginAccessIdentityList :: !CloudFrontOriginAccessIdentityList
    , _lcfoairStatus                             :: !Status
    } deriving (Eq,Show)

-- | 'ListCloudFrontOriginAccessIdentitiesResponse' smart constructor.
listCloudFrontOriginAccessIdentitiesResponse :: CloudFrontOriginAccessIdentityList -> Status -> ListCloudFrontOriginAccessIdentitiesResponse
listCloudFrontOriginAccessIdentitiesResponse pCloudFrontOriginAccessIdentityList pStatus =
    ListCloudFrontOriginAccessIdentitiesResponse'
    { _lcfoairCloudFrontOriginAccessIdentityList = pCloudFrontOriginAccessIdentityList
    , _lcfoairStatus = pStatus
    }

-- | The CloudFrontOriginAccessIdentityList type.
lcfoairCloudFrontOriginAccessIdentityList :: Lens' ListCloudFrontOriginAccessIdentitiesResponse CloudFrontOriginAccessIdentityList
lcfoairCloudFrontOriginAccessIdentityList = lens _lcfoairCloudFrontOriginAccessIdentityList (\ s a -> s{_lcfoairCloudFrontOriginAccessIdentityList = a});

-- | FIXME: Undocumented member.
lcfoairStatus :: Lens' ListCloudFrontOriginAccessIdentitiesResponse Status
lcfoairStatus = lens _lcfoairStatus (\ s a -> s{_lcfoairStatus = a});
