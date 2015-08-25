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
-- Module      : Network.AWS.CloudFront.ListInvalidations
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List invalidation batches.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/ListInvalidations.html AWS API Reference> for ListInvalidations.
module Network.AWS.CloudFront.ListInvalidations
    (
    -- * Creating a Request
      listInvalidations
    , ListInvalidations
    -- * Request Lenses
    , liMaxItems
    , liMarker
    , liDistributionId

    -- * Destructuring the Response
    , listInvalidationsResponse
    , ListInvalidationsResponse
    -- * Response Lenses
    , lirsStatus
    , lirsInvalidationList
    ) where

import           Network.AWS.CloudFront.Types
import           Network.AWS.CloudFront.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The request to list invalidations.
--
-- /See:/ 'listInvalidations' smart constructor.
data ListInvalidations = ListInvalidations'
    { _liMaxItems       :: !(Maybe Text)
    , _liMarker         :: !(Maybe Text)
    , _liDistributionId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListInvalidations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'liMaxItems'
--
-- * 'liMarker'
--
-- * 'liDistributionId'
listInvalidations
    :: Text -- ^ 'liDistributionId'
    -> ListInvalidations
listInvalidations pDistributionId_ =
    ListInvalidations'
    { _liMaxItems = Nothing
    , _liMarker = Nothing
    , _liDistributionId = pDistributionId_
    }

-- | The maximum number of invalidation batches you want in the response
-- body.
liMaxItems :: Lens' ListInvalidations (Maybe Text)
liMaxItems = lens _liMaxItems (\ s a -> s{_liMaxItems = a});

-- | Use this parameter when paginating results to indicate where to begin in
-- your list of invalidation batches. Because the results are returned in
-- decreasing order from most recent to oldest, the most recent results are
-- on the first page, the second page will contain earlier results, and so
-- on. To get the next page of results, set the Marker to the value of the
-- NextMarker from the current page\'s response. This value is the same as
-- the ID of the last invalidation batch on that page.
liMarker :: Lens' ListInvalidations (Maybe Text)
liMarker = lens _liMarker (\ s a -> s{_liMarker = a});

-- | The distribution\'s id.
liDistributionId :: Lens' ListInvalidations Text
liDistributionId = lens _liDistributionId (\ s a -> s{_liDistributionId = a});

instance AWSRequest ListInvalidations where
        type Rs ListInvalidations = ListInvalidationsResponse
        request = get cloudFront
        response
          = receiveXML
              (\ s h x ->
                 ListInvalidationsResponse' <$>
                   (pure (fromEnum s)) <*> (parseXML x))

instance ToHeaders ListInvalidations where
        toHeaders = const mempty

instance ToPath ListInvalidations where
        toPath ListInvalidations'{..}
          = mconcat
              ["/2015-04-17/distribution/", toBS _liDistributionId,
               "/invalidation"]

instance ToQuery ListInvalidations where
        toQuery ListInvalidations'{..}
          = mconcat
              ["MaxItems" =: _liMaxItems, "Marker" =: _liMarker]

-- | The returned result of the corresponding request.
--
-- /See:/ 'listInvalidationsResponse' smart constructor.
data ListInvalidationsResponse = ListInvalidationsResponse'
    { _lirsStatus           :: !Int
    , _lirsInvalidationList :: !InvalidationList
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListInvalidationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lirsStatus'
--
-- * 'lirsInvalidationList'
listInvalidationsResponse
    :: Int -- ^ 'lirsStatus'
    -> InvalidationList -- ^ 'lirsInvalidationList'
    -> ListInvalidationsResponse
listInvalidationsResponse pStatus_ pInvalidationList_ =
    ListInvalidationsResponse'
    { _lirsStatus = pStatus_
    , _lirsInvalidationList = pInvalidationList_
    }

-- | The response status code.
lirsStatus :: Lens' ListInvalidationsResponse Int
lirsStatus = lens _lirsStatus (\ s a -> s{_lirsStatus = a});

-- | Information about invalidation batches.
lirsInvalidationList :: Lens' ListInvalidationsResponse InvalidationList
lirsInvalidationList = lens _lirsInvalidationList (\ s a -> s{_lirsInvalidationList = a});
