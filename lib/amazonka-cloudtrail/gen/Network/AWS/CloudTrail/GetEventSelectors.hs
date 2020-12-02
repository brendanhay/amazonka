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
-- Module      : Network.AWS.CloudTrail.GetEventSelectors
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the settings for the event selectors that you configured for your trail. The information returned for your event selectors includes the following:
--
--
--     * The S3 objects that you are logging for data events.
--
--     * If your event selector includes management events.
--
--     * If your event selector includes read-only events, write-only events, or all.
--
--
--
-- For more information, see <http://docs.aws.amazon.com/awscloudtrail/latest/userguide/logging-management-and-data-events-with-cloudtrail.html Logging Data and Management Events for Trails > in the /AWS CloudTrail User Guide/ .
--
module Network.AWS.CloudTrail.GetEventSelectors
    (
    -- * Creating a Request
      getEventSelectors
    , GetEventSelectors
    -- * Request Lenses
    , gesTrailName

    -- * Destructuring the Response
    , getEventSelectorsResponse
    , GetEventSelectorsResponse
    -- * Response Lenses
    , gesrsTrailARN
    , gesrsEventSelectors
    , gesrsResponseStatus
    ) where

import Network.AWS.CloudTrail.Types
import Network.AWS.CloudTrail.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getEventSelectors' smart constructor.
newtype GetEventSelectors = GetEventSelectors'
  { _gesTrailName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetEventSelectors' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gesTrailName' - Specifies the name of the trail or trail ARN. If you specify a trail name, the string must meet the following requirements:     * Contain only ASCII letters (a-z, A-Z), numbers (0-9), periods (.), underscores (_), or dashes (-)     * Start with a letter or number, and end with a letter or number     * Be between 3 and 128 characters     * Have no adjacent periods, underscores or dashes. Names like @my-_namespace@ and @my--namespace@ are invalid.     * Not be in IP address format (for example, 192.168.5.4) If you specify a trail ARN, it must be in the format: @arn:aws:cloudtrail:us-east-1:123456789012:trail/MyTrail@
getEventSelectors
    :: Text -- ^ 'gesTrailName'
    -> GetEventSelectors
getEventSelectors pTrailName_ = GetEventSelectors' {_gesTrailName = pTrailName_}


-- | Specifies the name of the trail or trail ARN. If you specify a trail name, the string must meet the following requirements:     * Contain only ASCII letters (a-z, A-Z), numbers (0-9), periods (.), underscores (_), or dashes (-)     * Start with a letter or number, and end with a letter or number     * Be between 3 and 128 characters     * Have no adjacent periods, underscores or dashes. Names like @my-_namespace@ and @my--namespace@ are invalid.     * Not be in IP address format (for example, 192.168.5.4) If you specify a trail ARN, it must be in the format: @arn:aws:cloudtrail:us-east-1:123456789012:trail/MyTrail@
gesTrailName :: Lens' GetEventSelectors Text
gesTrailName = lens _gesTrailName (\ s a -> s{_gesTrailName = a})

instance AWSRequest GetEventSelectors where
        type Rs GetEventSelectors = GetEventSelectorsResponse
        request = postJSON cloudTrail
        response
          = receiveJSON
              (\ s h x ->
                 GetEventSelectorsResponse' <$>
                   (x .?> "TrailARN") <*>
                     (x .?> "EventSelectors" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetEventSelectors where

instance NFData GetEventSelectors where

instance ToHeaders GetEventSelectors where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.GetEventSelectors"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetEventSelectors where
        toJSON GetEventSelectors'{..}
          = object
              (catMaybes [Just ("TrailName" .= _gesTrailName)])

instance ToPath GetEventSelectors where
        toPath = const "/"

instance ToQuery GetEventSelectors where
        toQuery = const mempty

-- | /See:/ 'getEventSelectorsResponse' smart constructor.
data GetEventSelectorsResponse = GetEventSelectorsResponse'
  { _gesrsTrailARN       :: !(Maybe Text)
  , _gesrsEventSelectors :: !(Maybe [EventSelector])
  , _gesrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetEventSelectorsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gesrsTrailARN' - The specified trail ARN that has the event selectors.
--
-- * 'gesrsEventSelectors' - The event selectors that are configured for the trail.
--
-- * 'gesrsResponseStatus' - -- | The response status code.
getEventSelectorsResponse
    :: Int -- ^ 'gesrsResponseStatus'
    -> GetEventSelectorsResponse
getEventSelectorsResponse pResponseStatus_ =
  GetEventSelectorsResponse'
    { _gesrsTrailARN = Nothing
    , _gesrsEventSelectors = Nothing
    , _gesrsResponseStatus = pResponseStatus_
    }


-- | The specified trail ARN that has the event selectors.
gesrsTrailARN :: Lens' GetEventSelectorsResponse (Maybe Text)
gesrsTrailARN = lens _gesrsTrailARN (\ s a -> s{_gesrsTrailARN = a})

-- | The event selectors that are configured for the trail.
gesrsEventSelectors :: Lens' GetEventSelectorsResponse [EventSelector]
gesrsEventSelectors = lens _gesrsEventSelectors (\ s a -> s{_gesrsEventSelectors = a}) . _Default . _Coerce

-- | -- | The response status code.
gesrsResponseStatus :: Lens' GetEventSelectorsResponse Int
gesrsResponseStatus = lens _gesrsResponseStatus (\ s a -> s{_gesrsResponseStatus = a})

instance NFData GetEventSelectorsResponse where
