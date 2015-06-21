{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.SNS.ListEndpointsByPlatformApplication
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

-- | Lists the endpoints and endpoint attributes for devices in a supported
-- push notification service, such as GCM and APNS. The results for
-- @ListEndpointsByPlatformApplication@ are paginated and return a limited
-- list of endpoints, up to 100. If additional records are available after
-- the first page results, then a NextToken string will be returned. To
-- receive the next page, you call @ListEndpointsByPlatformApplication@
-- again using the NextToken string received from the previous call. When
-- there are no more records to return, NextToken will be null. For more
-- information, see
-- <http://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html Using Amazon SNS Mobile Push Notifications>.
--
-- <http://docs.aws.amazon.com/sns/latest/api/API_ListEndpointsByPlatformApplication.html>
module Network.AWS.SNS.ListEndpointsByPlatformApplication
    (
    -- * Request
      ListEndpointsByPlatformApplication
    -- ** Request constructor
    , listEndpointsByPlatformApplication
    -- ** Request lenses
    , lebpaNextToken
    , lebpaPlatformApplicationARN

    -- * Response
    , ListEndpointsByPlatformApplicationResponse
    -- ** Response constructor
    , listEndpointsByPlatformApplicationResponse
    -- ** Response lenses
    , lebparNextToken
    , lebparEndpoints
    ) where

import Network.AWS.Pagers
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SNS.Types

-- | /See:/ 'listEndpointsByPlatformApplication' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lebpaNextToken'
--
-- * 'lebpaPlatformApplicationARN'
data ListEndpointsByPlatformApplication = ListEndpointsByPlatformApplication'{_lebpaNextToken :: Maybe Text, _lebpaPlatformApplicationARN :: Text} deriving (Eq, Read, Show)

-- | 'ListEndpointsByPlatformApplication' smart constructor.
listEndpointsByPlatformApplication :: Text -> ListEndpointsByPlatformApplication
listEndpointsByPlatformApplication pPlatformApplicationARN = ListEndpointsByPlatformApplication'{_lebpaNextToken = Nothing, _lebpaPlatformApplicationARN = pPlatformApplicationARN};

-- | NextToken string is used when calling ListEndpointsByPlatformApplication
-- action to retrieve additional records that are available after the first
-- page results.
lebpaNextToken :: Lens' ListEndpointsByPlatformApplication (Maybe Text)
lebpaNextToken = lens _lebpaNextToken (\ s a -> s{_lebpaNextToken = a});

-- | PlatformApplicationArn for ListEndpointsByPlatformApplicationInput
-- action.
lebpaPlatformApplicationARN :: Lens' ListEndpointsByPlatformApplication Text
lebpaPlatformApplicationARN = lens _lebpaPlatformApplicationARN (\ s a -> s{_lebpaPlatformApplicationARN = a});

instance AWSPager ListEndpointsByPlatformApplication
         where
        page rq rs
          | stop (rs ^. lebparNextToken) = Nothing
          | otherwise =
            rq & lebpaNextToken ?~ rs ^. lebparNextToken

instance AWSRequest
         ListEndpointsByPlatformApplication where
        type Sv ListEndpointsByPlatformApplication = SNS
        type Rs ListEndpointsByPlatformApplication =
             ListEndpointsByPlatformApplicationResponse
        request = post
        response
          = receiveXMLWrapper
              "ListEndpointsByPlatformApplicationResult"
              (\ s h x ->
                 ListEndpointsByPlatformApplicationResponse' <$>
                   (x .@? "NextToken") <*>
                     (x .@? "Endpoints" .!@ mempty >>=
                        may (parseXMLList "member")))

instance ToHeaders ListEndpointsByPlatformApplication
         where
        toHeaders = const mempty

instance ToPath ListEndpointsByPlatformApplication
         where
        toPath = const "/"

instance ToQuery ListEndpointsByPlatformApplication
         where
        toQuery ListEndpointsByPlatformApplication'{..}
          = mconcat
              ["Action" =:
                 ("ListEndpointsByPlatformApplication" :: ByteString),
               "Version" =: ("2010-03-31" :: ByteString),
               "NextToken" =: _lebpaNextToken,
               "PlatformApplicationArn" =:
                 _lebpaPlatformApplicationARN]

-- | /See:/ 'listEndpointsByPlatformApplicationResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lebparNextToken'
--
-- * 'lebparEndpoints'
data ListEndpointsByPlatformApplicationResponse = ListEndpointsByPlatformApplicationResponse'{_lebparNextToken :: Maybe Text, _lebparEndpoints :: Maybe [Endpoint]} deriving (Eq, Read, Show)

-- | 'ListEndpointsByPlatformApplicationResponse' smart constructor.
listEndpointsByPlatformApplicationResponse :: ListEndpointsByPlatformApplicationResponse
listEndpointsByPlatformApplicationResponse = ListEndpointsByPlatformApplicationResponse'{_lebparNextToken = Nothing, _lebparEndpoints = Nothing};

-- | NextToken string is returned when calling
-- ListEndpointsByPlatformApplication action if additional records are
-- available after the first page results.
lebparNextToken :: Lens' ListEndpointsByPlatformApplicationResponse (Maybe Text)
lebparNextToken = lens _lebparNextToken (\ s a -> s{_lebparNextToken = a});

-- | Endpoints returned for ListEndpointsByPlatformApplication action.
lebparEndpoints :: Lens' ListEndpointsByPlatformApplicationResponse [Endpoint]
lebparEndpoints = lens _lebparEndpoints (\ s a -> s{_lebparEndpoints = a}) . _Default;
