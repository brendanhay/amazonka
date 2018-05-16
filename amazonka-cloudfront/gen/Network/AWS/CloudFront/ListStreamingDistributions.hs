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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List streaming distributions.
--
--
--
-- This operation returns paginated results.
module Network.AWS.CloudFront.ListStreamingDistributions
    (
    -- * Creating a Request
      listStreamingDistributions
    , ListStreamingDistributions
    -- * Request Lenses
    , lsdMarker
    , lsdMaxItems

    -- * Destructuring the Response
    , listStreamingDistributionsResponse
    , ListStreamingDistributionsResponse
    -- * Response Lenses
    , lsdrsResponseStatus
    , lsdrsStreamingDistributionList
    ) where

import Network.AWS.CloudFront.Types
import Network.AWS.CloudFront.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The request to list your streaming distributions.
--
--
--
-- /See:/ 'listStreamingDistributions' smart constructor.
data ListStreamingDistributions = ListStreamingDistributions'
  { _lsdMarker   :: !(Maybe Text)
  , _lsdMaxItems :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListStreamingDistributions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsdMarker' - The value that you provided for the @Marker@ request parameter.
--
-- * 'lsdMaxItems' - The value that you provided for the @MaxItems@ request parameter.
listStreamingDistributions
    :: ListStreamingDistributions
listStreamingDistributions =
  ListStreamingDistributions' {_lsdMarker = Nothing, _lsdMaxItems = Nothing}


-- | The value that you provided for the @Marker@ request parameter.
lsdMarker :: Lens' ListStreamingDistributions (Maybe Text)
lsdMarker = lens _lsdMarker (\ s a -> s{_lsdMarker = a})

-- | The value that you provided for the @MaxItems@ request parameter.
lsdMaxItems :: Lens' ListStreamingDistributions (Maybe Text)
lsdMaxItems = lens _lsdMaxItems (\ s a -> s{_lsdMaxItems = a})

instance AWSPager ListStreamingDistributions where
        page rq rs
          | stop
              (rs ^.
                 lsdrsStreamingDistributionList . sdlIsTruncated)
            = Nothing
          | isNothing
              (rs ^?
                 lsdrsStreamingDistributionList .
                   sdlNextMarker . _Just)
            = Nothing
          | otherwise =
            Just $ rq &
              lsdMarker .~
                rs ^?
                  lsdrsStreamingDistributionList .
                    sdlNextMarker . _Just

instance AWSRequest ListStreamingDistributions where
        type Rs ListStreamingDistributions =
             ListStreamingDistributionsResponse
        request = get cloudFront
        response
          = receiveXML
              (\ s h x ->
                 ListStreamingDistributionsResponse' <$>
                   (pure (fromEnum s)) <*> (parseXML x))

instance Hashable ListStreamingDistributions where

instance NFData ListStreamingDistributions where

instance ToHeaders ListStreamingDistributions where
        toHeaders = const mempty

instance ToPath ListStreamingDistributions where
        toPath = const "/2017-10-30/streaming-distribution"

instance ToQuery ListStreamingDistributions where
        toQuery ListStreamingDistributions'{..}
          = mconcat
              ["Marker" =: _lsdMarker, "MaxItems" =: _lsdMaxItems]

-- | The returned result of the corresponding request.
--
--
--
-- /See:/ 'listStreamingDistributionsResponse' smart constructor.
data ListStreamingDistributionsResponse = ListStreamingDistributionsResponse'
  { _lsdrsResponseStatus            :: !Int
  , _lsdrsStreamingDistributionList :: !StreamingDistributionList
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListStreamingDistributionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsdrsResponseStatus' - -- | The response status code.
--
-- * 'lsdrsStreamingDistributionList' - The @StreamingDistributionList@ type.
listStreamingDistributionsResponse
    :: Int -- ^ 'lsdrsResponseStatus'
    -> StreamingDistributionList -- ^ 'lsdrsStreamingDistributionList'
    -> ListStreamingDistributionsResponse
listStreamingDistributionsResponse pResponseStatus_ pStreamingDistributionList_ =
  ListStreamingDistributionsResponse'
    { _lsdrsResponseStatus = pResponseStatus_
    , _lsdrsStreamingDistributionList = pStreamingDistributionList_
    }


-- | -- | The response status code.
lsdrsResponseStatus :: Lens' ListStreamingDistributionsResponse Int
lsdrsResponseStatus = lens _lsdrsResponseStatus (\ s a -> s{_lsdrsResponseStatus = a})

-- | The @StreamingDistributionList@ type.
lsdrsStreamingDistributionList :: Lens' ListStreamingDistributionsResponse StreamingDistributionList
lsdrsStreamingDistributionList = lens _lsdrsStreamingDistributionList (\ s a -> s{_lsdrsStreamingDistributionList = a})

instance NFData ListStreamingDistributionsResponse
         where
