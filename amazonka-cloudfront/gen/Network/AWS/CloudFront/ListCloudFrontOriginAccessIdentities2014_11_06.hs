{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudFront.ListCloudFrontOriginAccessIdentities2014_11_06
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
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/ListCloudFrontOriginAccessIdentities2014_11_06.html>
module Network.AWS.CloudFront.ListCloudFrontOriginAccessIdentities2014_11_06
    (
    -- * Request
      ListCloudFrontOriginAccessIdentities2014_11_06
    -- ** Request constructor
    , listCloudFrontOriginAccessIdentities2014_11_06
    -- ** Request lenses
    , lcfoaiMaxItems
    , lcfoaiMarker

    -- * Response
    , ListCloudFrontOriginAccessIdentities2014_11_06Response
    -- ** Response constructor
    , listCloudFrontOriginAccessIdentities2014_11_06Response
    -- ** Response lenses
    , lcfoairCloudFrontOriginAccessIdentityList
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CloudFront.Types

-- | /See:/ 'listCloudFrontOriginAccessIdentities2014_11_06' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lcfoaiMaxItems'
--
-- * 'lcfoaiMarker'
data ListCloudFrontOriginAccessIdentities2014_11_06 = ListCloudFrontOriginAccessIdentities2014_11_06'{_lcfoaiMaxItems :: Maybe Text, _lcfoaiMarker :: Maybe Text} deriving (Eq, Read, Show)

-- | 'ListCloudFrontOriginAccessIdentities2014_11_06' smart constructor.
listCloudFrontOriginAccessIdentities2014_11_06 :: ListCloudFrontOriginAccessIdentities2014_11_06
listCloudFrontOriginAccessIdentities2014_11_06 = ListCloudFrontOriginAccessIdentities2014_11_06'{_lcfoaiMaxItems = Nothing, _lcfoaiMarker = Nothing};

-- | The maximum number of origin access identities you want in the response
-- body.
lcfoaiMaxItems :: Lens' ListCloudFrontOriginAccessIdentities2014_11_06 (Maybe Text)
lcfoaiMaxItems = lens _lcfoaiMaxItems (\ s a -> s{_lcfoaiMaxItems = a});

-- | Use this when paginating results to indicate where to begin in your list
-- of origin access identities. The results include identities in the list
-- that occur after the marker. To get the next page of results, set the
-- Marker to the value of the NextMarker from the current page\'s response
-- (which is also the ID of the last identity on that page).
lcfoaiMarker :: Lens' ListCloudFrontOriginAccessIdentities2014_11_06 (Maybe Text)
lcfoaiMarker = lens _lcfoaiMarker (\ s a -> s{_lcfoaiMarker = a});

instance AWSRequest
         ListCloudFrontOriginAccessIdentities2014_11_06 where
        type Sv
               ListCloudFrontOriginAccessIdentities2014_11_06
             = CloudFront
        type Rs
               ListCloudFrontOriginAccessIdentities2014_11_06
             =
             ListCloudFrontOriginAccessIdentities2014_11_06Response
        request = get
        response
          = receiveXML
              (\ s h x ->
                 ListCloudFrontOriginAccessIdentities2014_11_06Response'
                   <$> x .@ "CloudFrontOriginAccessIdentityList")

instance ToHeaders
         ListCloudFrontOriginAccessIdentities2014_11_06 where
        toHeaders = const mempty

instance ToPath
         ListCloudFrontOriginAccessIdentities2014_11_06 where
        toPath
          = const
              "/2014-11-06/origin-access-identity/cloudfront"

instance ToQuery
         ListCloudFrontOriginAccessIdentities2014_11_06 where
        toQuery
          ListCloudFrontOriginAccessIdentities2014_11_06'{..}
          = mconcat
              ["MaxItems" =: _lcfoaiMaxItems,
               "Marker" =: _lcfoaiMarker]

-- | /See:/ 'listCloudFrontOriginAccessIdentities2014_11_06Response' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lcfoairCloudFrontOriginAccessIdentityList'
newtype ListCloudFrontOriginAccessIdentities2014_11_06Response = ListCloudFrontOriginAccessIdentities2014_11_06Response'{_lcfoairCloudFrontOriginAccessIdentityList :: CloudFrontOriginAccessIdentityList} deriving (Eq, Read, Show)

-- | 'ListCloudFrontOriginAccessIdentities2014_11_06Response' smart constructor.
listCloudFrontOriginAccessIdentities2014_11_06Response :: CloudFrontOriginAccessIdentityList -> ListCloudFrontOriginAccessIdentities2014_11_06Response
listCloudFrontOriginAccessIdentities2014_11_06Response pCloudFrontOriginAccessIdentityList = ListCloudFrontOriginAccessIdentities2014_11_06Response'{_lcfoairCloudFrontOriginAccessIdentityList = pCloudFrontOriginAccessIdentityList};

-- | The CloudFrontOriginAccessIdentityList type.
lcfoairCloudFrontOriginAccessIdentityList :: Lens' ListCloudFrontOriginAccessIdentities2014_11_06Response CloudFrontOriginAccessIdentityList
lcfoairCloudFrontOriginAccessIdentityList = lens _lcfoairCloudFrontOriginAccessIdentityList (\ s a -> s{_lcfoairCloudFrontOriginAccessIdentityList = a});
