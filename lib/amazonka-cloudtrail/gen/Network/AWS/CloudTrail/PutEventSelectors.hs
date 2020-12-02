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
-- Module      : Network.AWS.CloudTrail.PutEventSelectors
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures an event selector for your trail. Use event selectors to specify whether you want your trail to log management and/or data events. When an event occurs in your account, CloudTrail evaluates the event selectors in all trails. For each trail, if the event matches any event selector, the trail processes and logs the event. If the event doesn't match any event selector, the trail doesn't log the event.
--
--
-- Example
--
--     * You create an event selector for a trail and specify that you want write-only events.
--
--     * The EC2 @GetConsoleOutput@ and @RunInstances@ API operations occur in your account.
--
--     * CloudTrail evaluates whether the events match your event selectors.
--
--     * The @RunInstances@ is a write-only event and it matches your event selector. The trail logs the event.
--
--     * The @GetConsoleOutput@ is a read-only event but it doesn't match your event selector. The trail doesn't log the event.
--
--
--
-- The @PutEventSelectors@ operation must be called from the region in which the trail was created; otherwise, an @InvalidHomeRegionException@ is thrown.
--
-- You can configure up to five event selectors for each trail. For more information, see <http://docs.aws.amazon.com/awscloudtrail/latest/userguide/logging-management-and-data-events-with-cloudtrail.html Logging Data and Management Events for Trails > in the /AWS CloudTrail User Guide/ .
--
module Network.AWS.CloudTrail.PutEventSelectors
    (
    -- * Creating a Request
      putEventSelectors
    , PutEventSelectors
    -- * Request Lenses
    , pesTrailName
    , pesEventSelectors

    -- * Destructuring the Response
    , putEventSelectorsResponse
    , PutEventSelectorsResponse
    -- * Response Lenses
    , pesrsTrailARN
    , pesrsEventSelectors
    , pesrsResponseStatus
    ) where

import Network.AWS.CloudTrail.Types
import Network.AWS.CloudTrail.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putEventSelectors' smart constructor.
data PutEventSelectors = PutEventSelectors'
  { _pesTrailName      :: !Text
  , _pesEventSelectors :: ![EventSelector]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutEventSelectors' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pesTrailName' - Specifies the name of the trail or trail ARN. If you specify a trail name, the string must meet the following requirements:     * Contain only ASCII letters (a-z, A-Z), numbers (0-9), periods (.), underscores (_), or dashes (-)     * Start with a letter or number, and end with a letter or number     * Be between 3 and 128 characters     * Have no adjacent periods, underscores or dashes. Names like @my-_namespace@ and @my--namespace@ are invalid.     * Not be in IP address format (for example, 192.168.5.4) If you specify a trail ARN, it must be in the format: @arn:aws:cloudtrail:us-east-1:123456789012:trail/MyTrail@
--
-- * 'pesEventSelectors' - Specifies the settings for your event selectors. You can configure up to five event selectors for a trail.
putEventSelectors
    :: Text -- ^ 'pesTrailName'
    -> PutEventSelectors
putEventSelectors pTrailName_ =
  PutEventSelectors' {_pesTrailName = pTrailName_, _pesEventSelectors = mempty}


-- | Specifies the name of the trail or trail ARN. If you specify a trail name, the string must meet the following requirements:     * Contain only ASCII letters (a-z, A-Z), numbers (0-9), periods (.), underscores (_), or dashes (-)     * Start with a letter or number, and end with a letter or number     * Be between 3 and 128 characters     * Have no adjacent periods, underscores or dashes. Names like @my-_namespace@ and @my--namespace@ are invalid.     * Not be in IP address format (for example, 192.168.5.4) If you specify a trail ARN, it must be in the format: @arn:aws:cloudtrail:us-east-1:123456789012:trail/MyTrail@
pesTrailName :: Lens' PutEventSelectors Text
pesTrailName = lens _pesTrailName (\ s a -> s{_pesTrailName = a})

-- | Specifies the settings for your event selectors. You can configure up to five event selectors for a trail.
pesEventSelectors :: Lens' PutEventSelectors [EventSelector]
pesEventSelectors = lens _pesEventSelectors (\ s a -> s{_pesEventSelectors = a}) . _Coerce

instance AWSRequest PutEventSelectors where
        type Rs PutEventSelectors = PutEventSelectorsResponse
        request = postJSON cloudTrail
        response
          = receiveJSON
              (\ s h x ->
                 PutEventSelectorsResponse' <$>
                   (x .?> "TrailARN") <*>
                     (x .?> "EventSelectors" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable PutEventSelectors where

instance NFData PutEventSelectors where

instance ToHeaders PutEventSelectors where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.PutEventSelectors"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutEventSelectors where
        toJSON PutEventSelectors'{..}
          = object
              (catMaybes
                 [Just ("TrailName" .= _pesTrailName),
                  Just ("EventSelectors" .= _pesEventSelectors)])

instance ToPath PutEventSelectors where
        toPath = const "/"

instance ToQuery PutEventSelectors where
        toQuery = const mempty

-- | /See:/ 'putEventSelectorsResponse' smart constructor.
data PutEventSelectorsResponse = PutEventSelectorsResponse'
  { _pesrsTrailARN       :: !(Maybe Text)
  , _pesrsEventSelectors :: !(Maybe [EventSelector])
  , _pesrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutEventSelectorsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pesrsTrailARN' - Specifies the ARN of the trail that was updated with event selectors. The format of a trail ARN is: @arn:aws:cloudtrail:us-east-1:123456789012:trail/MyTrail@
--
-- * 'pesrsEventSelectors' - Specifies the event selectors configured for your trail.
--
-- * 'pesrsResponseStatus' - -- | The response status code.
putEventSelectorsResponse
    :: Int -- ^ 'pesrsResponseStatus'
    -> PutEventSelectorsResponse
putEventSelectorsResponse pResponseStatus_ =
  PutEventSelectorsResponse'
    { _pesrsTrailARN = Nothing
    , _pesrsEventSelectors = Nothing
    , _pesrsResponseStatus = pResponseStatus_
    }


-- | Specifies the ARN of the trail that was updated with event selectors. The format of a trail ARN is: @arn:aws:cloudtrail:us-east-1:123456789012:trail/MyTrail@
pesrsTrailARN :: Lens' PutEventSelectorsResponse (Maybe Text)
pesrsTrailARN = lens _pesrsTrailARN (\ s a -> s{_pesrsTrailARN = a})

-- | Specifies the event selectors configured for your trail.
pesrsEventSelectors :: Lens' PutEventSelectorsResponse [EventSelector]
pesrsEventSelectors = lens _pesrsEventSelectors (\ s a -> s{_pesrsEventSelectors = a}) . _Default . _Coerce

-- | -- | The response status code.
pesrsResponseStatus :: Lens' PutEventSelectorsResponse Int
pesrsResponseStatus = lens _pesrsResponseStatus (\ s a -> s{_pesrsResponseStatus = a})

instance NFData PutEventSelectorsResponse where
