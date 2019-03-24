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
-- Module      : Network.AWS.EC2.GetConsoleScreenshot
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve a JPG-format screenshot of a running instance to help with troubleshooting.
--
--
-- The returned content is Base64-encoded.
--
module Network.AWS.EC2.GetConsoleScreenshot
    (
    -- * Creating a Request
      getConsoleScreenshot
    , GetConsoleScreenshot
    -- * Request Lenses
    , gcsWakeUp
    , gcsDryRun
    , gcsInstanceId

    -- * Destructuring the Response
    , getConsoleScreenshotResponse
    , GetConsoleScreenshotResponse
    -- * Response Lenses
    , gcsrsInstanceId
    , gcsrsImageData
    , gcsrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getConsoleScreenshot' smart constructor.
data GetConsoleScreenshot = GetConsoleScreenshot'
  { _gcsWakeUp     :: !(Maybe Bool)
  , _gcsDryRun     :: !(Maybe Bool)
  , _gcsInstanceId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetConsoleScreenshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcsWakeUp' - When set to @true@ , acts as keystroke input and wakes up an instance that's in standby or "sleep" mode.
--
-- * 'gcsDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'gcsInstanceId' - The ID of the instance.
getConsoleScreenshot
    :: Text -- ^ 'gcsInstanceId'
    -> GetConsoleScreenshot
getConsoleScreenshot pInstanceId_ =
  GetConsoleScreenshot'
    {_gcsWakeUp = Nothing, _gcsDryRun = Nothing, _gcsInstanceId = pInstanceId_}


-- | When set to @true@ , acts as keystroke input and wakes up an instance that's in standby or "sleep" mode.
gcsWakeUp :: Lens' GetConsoleScreenshot (Maybe Bool)
gcsWakeUp = lens _gcsWakeUp (\ s a -> s{_gcsWakeUp = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
gcsDryRun :: Lens' GetConsoleScreenshot (Maybe Bool)
gcsDryRun = lens _gcsDryRun (\ s a -> s{_gcsDryRun = a})

-- | The ID of the instance.
gcsInstanceId :: Lens' GetConsoleScreenshot Text
gcsInstanceId = lens _gcsInstanceId (\ s a -> s{_gcsInstanceId = a})

instance AWSRequest GetConsoleScreenshot where
        type Rs GetConsoleScreenshot =
             GetConsoleScreenshotResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 GetConsoleScreenshotResponse' <$>
                   (x .@? "instanceId") <*> (x .@? "imageData") <*>
                     (pure (fromEnum s)))

instance Hashable GetConsoleScreenshot where

instance NFData GetConsoleScreenshot where

instance ToHeaders GetConsoleScreenshot where
        toHeaders = const mempty

instance ToPath GetConsoleScreenshot where
        toPath = const "/"

instance ToQuery GetConsoleScreenshot where
        toQuery GetConsoleScreenshot'{..}
          = mconcat
              ["Action" =: ("GetConsoleScreenshot" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "WakeUp" =: _gcsWakeUp, "DryRun" =: _gcsDryRun,
               "InstanceId" =: _gcsInstanceId]

-- | /See:/ 'getConsoleScreenshotResponse' smart constructor.
data GetConsoleScreenshotResponse = GetConsoleScreenshotResponse'
  { _gcsrsInstanceId     :: !(Maybe Text)
  , _gcsrsImageData      :: !(Maybe Text)
  , _gcsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetConsoleScreenshotResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcsrsInstanceId' - The ID of the instance.
--
-- * 'gcsrsImageData' - The data that comprises the image.
--
-- * 'gcsrsResponseStatus' - -- | The response status code.
getConsoleScreenshotResponse
    :: Int -- ^ 'gcsrsResponseStatus'
    -> GetConsoleScreenshotResponse
getConsoleScreenshotResponse pResponseStatus_ =
  GetConsoleScreenshotResponse'
    { _gcsrsInstanceId = Nothing
    , _gcsrsImageData = Nothing
    , _gcsrsResponseStatus = pResponseStatus_
    }


-- | The ID of the instance.
gcsrsInstanceId :: Lens' GetConsoleScreenshotResponse (Maybe Text)
gcsrsInstanceId = lens _gcsrsInstanceId (\ s a -> s{_gcsrsInstanceId = a})

-- | The data that comprises the image.
gcsrsImageData :: Lens' GetConsoleScreenshotResponse (Maybe Text)
gcsrsImageData = lens _gcsrsImageData (\ s a -> s{_gcsrsImageData = a})

-- | -- | The response status code.
gcsrsResponseStatus :: Lens' GetConsoleScreenshotResponse Int
gcsrsResponseStatus = lens _gcsrsResponseStatus (\ s a -> s{_gcsrsResponseStatus = a})

instance NFData GetConsoleScreenshotResponse where
