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
-- Module      : Network.AWS.CloudFront.ListStreamingDistributions
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List streaming distributions.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/ListStreamingDistributions.html AWS API Reference> for ListStreamingDistributions.
module Network.AWS.CloudFront.ListStreamingDistributions
    (
    -- * Creating a Request
      listStreamingDistributions
    , ListStreamingDistributions
    -- * Request Lenses
    , lsdMaxItems
    , lsdMarker

    -- * Destructuring the Response
    , listStreamingDistributionsResponse
    , ListStreamingDistributionsResponse
    -- * Response Lenses
    , lsdrsStatus
    , lsdrsStreamingDistributionList
    ) where

import           Network.AWS.CloudFront.Types
import           Network.AWS.CloudFront.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The request to list your streaming distributions.
--
-- /See:/ 'listStreamingDistributions' smart constructor.
data ListStreamingDistributions = ListStreamingDistributions'
    { _lsdMaxItems :: !(Maybe Text)
    , _lsdMarker   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListStreamingDistributions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsdMaxItems'
--
-- * 'lsdMarker'
listStreamingDistributions
    :: ListStreamingDistributions
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
        type Rs ListStreamingDistributions =
             ListStreamingDistributionsResponse
        request = get cloudFront
        response
          = receiveXML
              (\ s h x ->
                 ListStreamingDistributionsResponse' <$>
                   (pure (fromEnum s)) <*> (parseXML x))

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
data ListStreamingDistributionsResponse = ListStreamingDistributionsResponse'
    { _lsdrsStatus                    :: !Int
    , _lsdrsStreamingDistributionList :: !StreamingDistributionList
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListStreamingDistributionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsdrsStatus'
--
-- * 'lsdrsStreamingDistributionList'
listStreamingDistributionsResponse
    :: Int -- ^ 'lsdrsStatus'
    -> StreamingDistributionList -- ^ 'lsdrsStreamingDistributionList'
    -> ListStreamingDistributionsResponse
listStreamingDistributionsResponse pStatus_ pStreamingDistributionList_ =
    ListStreamingDistributionsResponse'
    { _lsdrsStatus = pStatus_
    , _lsdrsStreamingDistributionList = pStreamingDistributionList_
    }

-- | The response status code.
lsdrsStatus :: Lens' ListStreamingDistributionsResponse Int
lsdrsStatus = lens _lsdrsStatus (\ s a -> s{_lsdrsStatus = a});

-- | The StreamingDistributionList type.
lsdrsStreamingDistributionList :: Lens' ListStreamingDistributionsResponse StreamingDistributionList
lsdrsStreamingDistributionList = lens _lsdrsStreamingDistributionList (\ s a -> s{_lsdrsStreamingDistributionList = a});
