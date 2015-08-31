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
-- Module      : Network.AWS.CloudFront.ListCloudFrontOriginAccessIdentities
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List origin access identities.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/ListCloudFrontOriginAccessIdentities.html AWS API Reference> for ListCloudFrontOriginAccessIdentities.
module Network.AWS.CloudFront.ListCloudFrontOriginAccessIdentities
    (
    -- * Creating a Request
      listCloudFrontOriginAccessIdentities
    , ListCloudFrontOriginAccessIdentities
    -- * Request Lenses
    , lcfoaiMarker
    , lcfoaiMaxItems

    -- * Destructuring the Response
    , listCloudFrontOriginAccessIdentitiesResponse
    , ListCloudFrontOriginAccessIdentitiesResponse
    -- * Response Lenses
    , lcfoairsStatus
    , lcfoairsCloudFrontOriginAccessIdentityList
    ) where

import           Network.AWS.CloudFront.Types
import           Network.AWS.CloudFront.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The request to list origin access identities.
--
-- /See:/ 'listCloudFrontOriginAccessIdentities' smart constructor.
data ListCloudFrontOriginAccessIdentities = ListCloudFrontOriginAccessIdentities'
    { _lcfoaiMarker   :: !(Maybe Text)
    , _lcfoaiMaxItems :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListCloudFrontOriginAccessIdentities' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcfoaiMarker'
--
-- * 'lcfoaiMaxItems'
listCloudFrontOriginAccessIdentities
    :: ListCloudFrontOriginAccessIdentities
listCloudFrontOriginAccessIdentities =
    ListCloudFrontOriginAccessIdentities'
    { _lcfoaiMarker = Nothing
    , _lcfoaiMaxItems = Nothing
    }

-- | Use this when paginating results to indicate where to begin in your list
-- of origin access identities. The results include identities in the list
-- that occur after the marker. To get the next page of results, set the
-- Marker to the value of the NextMarker from the current page\'s response
-- (which is also the ID of the last identity on that page).
lcfoaiMarker :: Lens' ListCloudFrontOriginAccessIdentities (Maybe Text)
lcfoaiMarker = lens _lcfoaiMarker (\ s a -> s{_lcfoaiMarker = a});

-- | The maximum number of origin access identities you want in the response
-- body.
lcfoaiMaxItems :: Lens' ListCloudFrontOriginAccessIdentities (Maybe Text)
lcfoaiMaxItems = lens _lcfoaiMaxItems (\ s a -> s{_lcfoaiMaxItems = a});

instance AWSRequest
         ListCloudFrontOriginAccessIdentities where
        type Rs ListCloudFrontOriginAccessIdentities =
             ListCloudFrontOriginAccessIdentitiesResponse
        request = get cloudFront
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
              "/2015-04-17/origin-access-identity/cloudfront"

instance ToQuery ListCloudFrontOriginAccessIdentities
         where
        toQuery ListCloudFrontOriginAccessIdentities'{..}
          = mconcat
              ["Marker" =: _lcfoaiMarker,
               "MaxItems" =: _lcfoaiMaxItems]

-- | The returned result of the corresponding request.
--
-- /See:/ 'listCloudFrontOriginAccessIdentitiesResponse' smart constructor.
data ListCloudFrontOriginAccessIdentitiesResponse = ListCloudFrontOriginAccessIdentitiesResponse'
    { _lcfoairsStatus                             :: !Int
    , _lcfoairsCloudFrontOriginAccessIdentityList :: !CloudFrontOriginAccessIdentityList
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListCloudFrontOriginAccessIdentitiesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcfoairsStatus'
--
-- * 'lcfoairsCloudFrontOriginAccessIdentityList'
listCloudFrontOriginAccessIdentitiesResponse
    :: Int -- ^ 'lcfoairsStatus'
    -> CloudFrontOriginAccessIdentityList -- ^ 'lcfoairsCloudFrontOriginAccessIdentityList'
    -> ListCloudFrontOriginAccessIdentitiesResponse
listCloudFrontOriginAccessIdentitiesResponse pStatus_ pCloudFrontOriginAccessIdentityList_ =
    ListCloudFrontOriginAccessIdentitiesResponse'
    { _lcfoairsStatus = pStatus_
    , _lcfoairsCloudFrontOriginAccessIdentityList = pCloudFrontOriginAccessIdentityList_
    }

-- | The response status code.
lcfoairsStatus :: Lens' ListCloudFrontOriginAccessIdentitiesResponse Int
lcfoairsStatus = lens _lcfoairsStatus (\ s a -> s{_lcfoairsStatus = a});

-- | The CloudFrontOriginAccessIdentityList type.
lcfoairsCloudFrontOriginAccessIdentityList :: Lens' ListCloudFrontOriginAccessIdentitiesResponse CloudFrontOriginAccessIdentityList
lcfoairsCloudFrontOriginAccessIdentityList = lens _lcfoairsCloudFrontOriginAccessIdentityList (\ s a -> s{_lcfoairsCloudFrontOriginAccessIdentityList = a});
