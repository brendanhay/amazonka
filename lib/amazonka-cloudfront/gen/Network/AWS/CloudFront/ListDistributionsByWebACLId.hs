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
-- Module      : Network.AWS.CloudFront.ListDistributionsByWebACLId
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the distributions that are associated with a specified AWS WAF web ACL.
--
--
module Network.AWS.CloudFront.ListDistributionsByWebACLId
    (
    -- * Creating a Request
      listDistributionsByWebACLId
    , ListDistributionsByWebACLId
    -- * Request Lenses
    , ldbwaiMarker
    , ldbwaiMaxItems
    , ldbwaiWebACLId

    -- * Destructuring the Response
    , listDistributionsByWebACLIdResponse
    , ListDistributionsByWebACLIdResponse
    -- * Response Lenses
    , ldbwairsDistributionList
    , ldbwairsResponseStatus
    ) where

import Network.AWS.CloudFront.Types
import Network.AWS.CloudFront.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The request to list distributions that are associated with a specified AWS WAF web ACL.
--
--
--
-- /See:/ 'listDistributionsByWebACLId' smart constructor.
data ListDistributionsByWebACLId = ListDistributionsByWebACLId'
  { _ldbwaiMarker   :: !(Maybe Text)
  , _ldbwaiMaxItems :: !(Maybe Text)
  , _ldbwaiWebACLId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListDistributionsByWebACLId' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldbwaiMarker' - Use @Marker@ and @MaxItems@ to control pagination of results. If you have more than @MaxItems@ distributions that satisfy the request, the response includes a @NextMarker@ element. To get the next page of results, submit another request. For the value of @Marker@ , specify the value of @NextMarker@ from the last response. (For the first request, omit @Marker@ .)
--
-- * 'ldbwaiMaxItems' - The maximum number of distributions that you want CloudFront to return in the response body. The maximum and default values are both 100.
--
-- * 'ldbwaiWebACLId' - The ID of the AWS WAF web ACL that you want to list the associated distributions. If you specify "null" for the ID, the request returns a list of the distributions that aren't associated with a web ACL.
listDistributionsByWebACLId
    :: Text -- ^ 'ldbwaiWebACLId'
    -> ListDistributionsByWebACLId
listDistributionsByWebACLId pWebACLId_ =
  ListDistributionsByWebACLId'
    { _ldbwaiMarker = Nothing
    , _ldbwaiMaxItems = Nothing
    , _ldbwaiWebACLId = pWebACLId_
    }


-- | Use @Marker@ and @MaxItems@ to control pagination of results. If you have more than @MaxItems@ distributions that satisfy the request, the response includes a @NextMarker@ element. To get the next page of results, submit another request. For the value of @Marker@ , specify the value of @NextMarker@ from the last response. (For the first request, omit @Marker@ .)
ldbwaiMarker :: Lens' ListDistributionsByWebACLId (Maybe Text)
ldbwaiMarker = lens _ldbwaiMarker (\ s a -> s{_ldbwaiMarker = a})

-- | The maximum number of distributions that you want CloudFront to return in the response body. The maximum and default values are both 100.
ldbwaiMaxItems :: Lens' ListDistributionsByWebACLId (Maybe Text)
ldbwaiMaxItems = lens _ldbwaiMaxItems (\ s a -> s{_ldbwaiMaxItems = a})

-- | The ID of the AWS WAF web ACL that you want to list the associated distributions. If you specify "null" for the ID, the request returns a list of the distributions that aren't associated with a web ACL.
ldbwaiWebACLId :: Lens' ListDistributionsByWebACLId Text
ldbwaiWebACLId = lens _ldbwaiWebACLId (\ s a -> s{_ldbwaiWebACLId = a})

instance AWSRequest ListDistributionsByWebACLId where
        type Rs ListDistributionsByWebACLId =
             ListDistributionsByWebACLIdResponse
        request = get cloudFront
        response
          = receiveXML
              (\ s h x ->
                 ListDistributionsByWebACLIdResponse' <$>
                   (parseXML x) <*> (pure (fromEnum s)))

instance Hashable ListDistributionsByWebACLId where

instance NFData ListDistributionsByWebACLId where

instance ToHeaders ListDistributionsByWebACLId where
        toHeaders = const mempty

instance ToPath ListDistributionsByWebACLId where
        toPath ListDistributionsByWebACLId'{..}
          = mconcat
              ["/2017-10-30/distributionsByWebACLId/",
               toBS _ldbwaiWebACLId]

instance ToQuery ListDistributionsByWebACLId where
        toQuery ListDistributionsByWebACLId'{..}
          = mconcat
              ["Marker" =: _ldbwaiMarker,
               "MaxItems" =: _ldbwaiMaxItems]

-- | The response to a request to list the distributions that are associated with a specified AWS WAF web ACL.
--
--
--
-- /See:/ 'listDistributionsByWebACLIdResponse' smart constructor.
data ListDistributionsByWebACLIdResponse = ListDistributionsByWebACLIdResponse'
  { _ldbwairsDistributionList :: !(Maybe DistributionList)
  , _ldbwairsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListDistributionsByWebACLIdResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldbwairsDistributionList' - The @DistributionList@ type.
--
-- * 'ldbwairsResponseStatus' - -- | The response status code.
listDistributionsByWebACLIdResponse
    :: Int -- ^ 'ldbwairsResponseStatus'
    -> ListDistributionsByWebACLIdResponse
listDistributionsByWebACLIdResponse pResponseStatus_ =
  ListDistributionsByWebACLIdResponse'
    { _ldbwairsDistributionList = Nothing
    , _ldbwairsResponseStatus = pResponseStatus_
    }


-- | The @DistributionList@ type.
ldbwairsDistributionList :: Lens' ListDistributionsByWebACLIdResponse (Maybe DistributionList)
ldbwairsDistributionList = lens _ldbwairsDistributionList (\ s a -> s{_ldbwairsDistributionList = a})

-- | -- | The response status code.
ldbwairsResponseStatus :: Lens' ListDistributionsByWebACLIdResponse Int
ldbwairsResponseStatus = lens _ldbwairsResponseStatus (\ s a -> s{_ldbwairsResponseStatus = a})

instance NFData ListDistributionsByWebACLIdResponse
         where
