{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.ListCloudFrontOriginAccessIdentities
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- List origin access identities.
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
    , lcfoairsStatus
    , lcfoairsCloudFrontOriginAccessIdentityList
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
                   (pure (fromEnum s)) <*> (parseXML x))

instance ToHeaders
         ListCloudFrontOriginAccessIdentities where
        toHeaders = const mempty

instance ToPath ListCloudFrontOriginAccessIdentities
         where
        toPath
          = const
              ["2015-04-17", "origin-access-identity",
               "cloudfront"]

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
-- * 'lcfoairsStatus'
--
-- * 'lcfoairsCloudFrontOriginAccessIdentityList'
data ListCloudFrontOriginAccessIdentitiesResponse = ListCloudFrontOriginAccessIdentitiesResponse'
    { _lcfoairsStatus                             :: !Int
    , _lcfoairsCloudFrontOriginAccessIdentityList :: !CloudFrontOriginAccessIdentityList
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListCloudFrontOriginAccessIdentitiesResponse' smart constructor.
listCloudFrontOriginAccessIdentitiesResponse :: Int -> CloudFrontOriginAccessIdentityList -> ListCloudFrontOriginAccessIdentitiesResponse
listCloudFrontOriginAccessIdentitiesResponse pStatus_ pCloudFrontOriginAccessIdentityList_ =
    ListCloudFrontOriginAccessIdentitiesResponse'
    { _lcfoairsStatus = pStatus_
    , _lcfoairsCloudFrontOriginAccessIdentityList = pCloudFrontOriginAccessIdentityList_
    }

-- | FIXME: Undocumented member.
lcfoairsStatus :: Lens' ListCloudFrontOriginAccessIdentitiesResponse Int
lcfoairsStatus = lens _lcfoairsStatus (\ s a -> s{_lcfoairsStatus = a});

-- | The CloudFrontOriginAccessIdentityList type.
lcfoairsCloudFrontOriginAccessIdentityList :: Lens' ListCloudFrontOriginAccessIdentitiesResponse CloudFrontOriginAccessIdentityList
lcfoairsCloudFrontOriginAccessIdentityList = lens _lcfoairsCloudFrontOriginAccessIdentityList (\ s a -> s{_lcfoairsCloudFrontOriginAccessIdentityList = a});
