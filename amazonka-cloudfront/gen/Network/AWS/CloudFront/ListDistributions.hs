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
-- Module      : Network.AWS.CloudFront.ListDistributions
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List distributions.
--
-- This operation returns paginated results.
module Network.AWS.CloudFront.ListDistributions
    (
    -- * Creating a Request
      listDistributions
    , ListDistributions
    -- * Request Lenses
    , ldMarker
    , ldMaxItems

    -- * Destructuring the Response
    , listDistributionsResponse
    , ListDistributionsResponse
    -- * Response Lenses
    , ldrsResponseStatus
    , ldrsDistributionList
    ) where

import           Network.AWS.CloudFront.Types
import           Network.AWS.CloudFront.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The request to list your distributions.
--
-- /See:/ 'listDistributions' smart constructor.
data ListDistributions = ListDistributions'
    { _ldMarker   :: !(Maybe Text)
    , _ldMaxItems :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListDistributions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldMarker'
--
-- * 'ldMaxItems'
listDistributions
    :: ListDistributions
listDistributions =
    ListDistributions'
    { _ldMarker = Nothing
    , _ldMaxItems = Nothing
    }

-- | Use Marker and MaxItems to control pagination of results. If you have more than MaxItems distributions that satisfy the request, the response includes a NextMarker element. To get the next page of results, submit another request. For the value of Marker, specify the value of NextMarker from the last response. (For the first request, omit Marker.)
ldMarker :: Lens' ListDistributions (Maybe Text)
ldMarker = lens _ldMarker (\ s a -> s{_ldMarker = a});

-- | The maximum number of distributions that you want CloudFront to return in the response body. The maximum and default values are both 100.
ldMaxItems :: Lens' ListDistributions (Maybe Text)
ldMaxItems = lens _ldMaxItems (\ s a -> s{_ldMaxItems = a});

instance AWSPager ListDistributions where
        page rq rs
          | stop (rs ^. ldrsDistributionList . dlIsTruncated) =
            Nothing
          | isNothing
              (rs ^? ldrsDistributionList . dlNextMarker . _Just)
            = Nothing
          | otherwise =
            Just $ rq &
              ldMarker .~
                rs ^? ldrsDistributionList . dlNextMarker . _Just

instance AWSRequest ListDistributions where
        type Rs ListDistributions = ListDistributionsResponse
        request = get cloudFront
        response
          = receiveXML
              (\ s h x ->
                 ListDistributionsResponse' <$>
                   (pure (fromEnum s)) <*> (parseXML x))

instance Hashable ListDistributions

instance NFData ListDistributions

instance ToHeaders ListDistributions where
        toHeaders = const mempty

instance ToPath ListDistributions where
        toPath = const "/2016-09-07/distribution"

instance ToQuery ListDistributions where
        toQuery ListDistributions'{..}
          = mconcat
              ["Marker" =: _ldMarker, "MaxItems" =: _ldMaxItems]

-- | The returned result of the corresponding request.
--
-- /See:/ 'listDistributionsResponse' smart constructor.
data ListDistributionsResponse = ListDistributionsResponse'
    { _ldrsResponseStatus   :: !Int
    , _ldrsDistributionList :: !DistributionList
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListDistributionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldrsResponseStatus'
--
-- * 'ldrsDistributionList'
listDistributionsResponse
    :: Int -- ^ 'ldrsResponseStatus'
    -> DistributionList -- ^ 'ldrsDistributionList'
    -> ListDistributionsResponse
listDistributionsResponse pResponseStatus_ pDistributionList_ =
    ListDistributionsResponse'
    { _ldrsResponseStatus = pResponseStatus_
    , _ldrsDistributionList = pDistributionList_
    }

-- | The response status code.
ldrsResponseStatus :: Lens' ListDistributionsResponse Int
ldrsResponseStatus = lens _ldrsResponseStatus (\ s a -> s{_ldrsResponseStatus = a});

-- | The DistributionList type.
ldrsDistributionList :: Lens' ListDistributionsResponse DistributionList
ldrsDistributionList = lens _ldrsDistributionList (\ s a -> s{_ldrsDistributionList = a});

instance NFData ListDistributionsResponse
