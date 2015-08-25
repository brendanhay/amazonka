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
-- Module      : Network.AWS.SNS.ListEndpointsByPlatformApplication
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the endpoints and endpoint attributes for devices in a supported
-- push notification service, such as GCM and APNS. The results for
-- 'ListEndpointsByPlatformApplication' are paginated and return a limited
-- list of endpoints, up to 100. If additional records are available after
-- the first page results, then a NextToken string will be returned. To
-- receive the next page, you call 'ListEndpointsByPlatformApplication'
-- again using the NextToken string received from the previous call. When
-- there are no more records to return, NextToken will be null. For more
-- information, see
-- <http://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html Using Amazon SNS Mobile Push Notifications>.
--
-- /See:/ <http://docs.aws.amazon.com/sns/latest/api/API_ListEndpointsByPlatformApplication.html AWS API Reference> for ListEndpointsByPlatformApplication.
--
-- This operation returns paginated results.
module Network.AWS.SNS.ListEndpointsByPlatformApplication
    (
    -- * Creating a Request
      listEndpointsByPlatformApplication
    , ListEndpointsByPlatformApplication
    -- * Request Lenses
    , lebpaNextToken
    , lebpaPlatformApplicationARN

    -- * Destructuring the Response
    , listEndpointsByPlatformApplicationResponse
    , ListEndpointsByPlatformApplicationResponse
    -- * Response Lenses
    , lebparsNextToken
    , lebparsEndpoints
    , lebparsStatus
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SNS.Types
import           Network.AWS.SNS.Types.Product

-- | Input for ListEndpointsByPlatformApplication action.
--
-- /See:/ 'listEndpointsByPlatformApplication' smart constructor.
data ListEndpointsByPlatformApplication = ListEndpointsByPlatformApplication'
    { _lebpaNextToken              :: !(Maybe Text)
    , _lebpaPlatformApplicationARN :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListEndpointsByPlatformApplication' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lebpaNextToken'
--
-- * 'lebpaPlatformApplicationARN'
listEndpointsByPlatformApplication
    :: Text -- ^ 'lebpaPlatformApplicationARN'
    -> ListEndpointsByPlatformApplication
listEndpointsByPlatformApplication pPlatformApplicationARN_ =
    ListEndpointsByPlatformApplication'
    { _lebpaNextToken = Nothing
    , _lebpaPlatformApplicationARN = pPlatformApplicationARN_
    }

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
          | stop (rs ^. lebparsNextToken) = Nothing
          | stop (rs ^. lebparsEndpoints) = Nothing
          | otherwise =
            Just $ rq & lebpaNextToken .~ rs ^. lebparsNextToken

instance AWSRequest
         ListEndpointsByPlatformApplication where
        type Rs ListEndpointsByPlatformApplication =
             ListEndpointsByPlatformApplicationResponse
        request = postQuery sNS
        response
          = receiveXMLWrapper
              "ListEndpointsByPlatformApplicationResult"
              (\ s h x ->
                 ListEndpointsByPlatformApplicationResponse' <$>
                   (x .@? "NextToken") <*>
                     (x .@? "Endpoints" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

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

-- | Response for ListEndpointsByPlatformApplication action.
--
-- /See:/ 'listEndpointsByPlatformApplicationResponse' smart constructor.
data ListEndpointsByPlatformApplicationResponse = ListEndpointsByPlatformApplicationResponse'
    { _lebparsNextToken :: !(Maybe Text)
    , _lebparsEndpoints :: !(Maybe [Endpoint])
    , _lebparsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListEndpointsByPlatformApplicationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lebparsNextToken'
--
-- * 'lebparsEndpoints'
--
-- * 'lebparsStatus'
listEndpointsByPlatformApplicationResponse
    :: Int -- ^ 'lebparsStatus'
    -> ListEndpointsByPlatformApplicationResponse
listEndpointsByPlatformApplicationResponse pStatus_ =
    ListEndpointsByPlatformApplicationResponse'
    { _lebparsNextToken = Nothing
    , _lebparsEndpoints = Nothing
    , _lebparsStatus = pStatus_
    }

-- | NextToken string is returned when calling
-- ListEndpointsByPlatformApplication action if additional records are
-- available after the first page results.
lebparsNextToken :: Lens' ListEndpointsByPlatformApplicationResponse (Maybe Text)
lebparsNextToken = lens _lebparsNextToken (\ s a -> s{_lebparsNextToken = a});

-- | Endpoints returned for ListEndpointsByPlatformApplication action.
lebparsEndpoints :: Lens' ListEndpointsByPlatformApplicationResponse [Endpoint]
lebparsEndpoints = lens _lebparsEndpoints (\ s a -> s{_lebparsEndpoints = a}) . _Default . _Coerce;

-- | The response status code.
lebparsStatus :: Lens' ListEndpointsByPlatformApplicationResponse Int
lebparsStatus = lens _lebparsStatus (\ s a -> s{_lebparsStatus = a});
