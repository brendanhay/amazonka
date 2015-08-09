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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List distributions.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/ListDistributions.html AWS API Reference> for ListDistributions.
module Network.AWS.CloudFront.ListDistributions
    (
    -- * Creating a Request
      listDistributions
    , ListDistributions
    -- * Request Lenses
    , ldMaxItems
    , ldMarker

    -- * Destructuring the Response
    , listDistributionsResponse
    , ListDistributionsResponse
    -- * Response Lenses
    , ldrsStatus
    , ldrsDistributionList
    ) where

import           Network.AWS.CloudFront.Types
import           Network.AWS.CloudFront.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The request to list your distributions.
--
-- /See:/ 'listDistributions' smart constructor.
data ListDistributions = ListDistributions'
    { _ldMaxItems :: !(Maybe Text)
    , _ldMarker   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListDistributions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldMaxItems'
--
-- * 'ldMarker'
listDistributions
    :: ListDistributions
listDistributions =
    ListDistributions'
    { _ldMaxItems = Nothing
    , _ldMarker = Nothing
    }

-- | The maximum number of distributions you want in the response body.
ldMaxItems :: Lens' ListDistributions (Maybe Text)
ldMaxItems = lens _ldMaxItems (\ s a -> s{_ldMaxItems = a});

-- | Use this when paginating results to indicate where to begin in your list
-- of distributions. The results include distributions in the list that
-- occur after the marker. To get the next page of results, set the Marker
-- to the value of the NextMarker from the current page\'s response (which
-- is also the ID of the last distribution on that page).
ldMarker :: Lens' ListDistributions (Maybe Text)
ldMarker = lens _ldMarker (\ s a -> s{_ldMarker = a});

instance AWSRequest ListDistributions where
        type Sv ListDistributions = CloudFront
        type Rs ListDistributions = ListDistributionsResponse
        request = get
        response
          = receiveXML
              (\ s h x ->
                 ListDistributionsResponse' <$>
                   (pure (fromEnum s)) <*> (parseXML x))

instance ToHeaders ListDistributions where
        toHeaders = const mempty

instance ToPath ListDistributions where
        toPath = const "/2015-04-17/distribution"

instance ToQuery ListDistributions where
        toQuery ListDistributions'{..}
          = mconcat
              ["MaxItems" =: _ldMaxItems, "Marker" =: _ldMarker]

-- | The returned result of the corresponding request.
--
-- /See:/ 'listDistributionsResponse' smart constructor.
data ListDistributionsResponse = ListDistributionsResponse'
    { _ldrsStatus           :: !Int
    , _ldrsDistributionList :: !DistributionList
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListDistributionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldrsStatus'
--
-- * 'ldrsDistributionList'
listDistributionsResponse
    :: Int -- ^ 'ldrsStatus'
    -> DistributionList -- ^ 'ldrsDistributionList'
    -> ListDistributionsResponse
listDistributionsResponse pStatus_ pDistributionList_ =
    ListDistributionsResponse'
    { _ldrsStatus = pStatus_
    , _ldrsDistributionList = pDistributionList_
    }

-- | The response status code.
ldrsStatus :: Lens' ListDistributionsResponse Int
ldrsStatus = lens _ldrsStatus (\ s a -> s{_ldrsStatus = a});

-- | The DistributionList type.
ldrsDistributionList :: Lens' ListDistributionsResponse DistributionList
ldrsDistributionList = lens _ldrsDistributionList (\ s a -> s{_ldrsDistributionList = a});
