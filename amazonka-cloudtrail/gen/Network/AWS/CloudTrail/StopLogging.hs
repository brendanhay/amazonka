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
-- Module      : Network.AWS.CloudTrail.StopLogging
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Suspends the recording of AWS API calls and log file delivery for the specified trail. Under most circumstances, there is no need to use this action. You can update a trail without stopping it first. This action is the only way to stop recording. For a trail enabled in all regions, this operation must be called from the region in which the trail was created, or an @InvalidHomeRegionException@ will occur. This operation cannot be called on the shadow trails (replicated trails in other regions) of a trail enabled in all regions.
--
--
module Network.AWS.CloudTrail.StopLogging
    (
    -- * Creating a Request
      stopLogging
    , StopLogging
    -- * Request Lenses
    , slName

    -- * Destructuring the Response
    , stopLoggingResponse
    , StopLoggingResponse
    -- * Response Lenses
    , slrsResponseStatus
    ) where

import Network.AWS.CloudTrail.Types
import Network.AWS.CloudTrail.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Passes the request to CloudTrail to stop logging AWS API calls for the specified account.
--
--
--
-- /See:/ 'stopLogging' smart constructor.
newtype StopLogging = StopLogging'
  { _slName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopLogging' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slName' - Specifies the name or the CloudTrail ARN of the trail for which CloudTrail will stop logging AWS API calls. The format of a trail ARN is: @arn:aws:cloudtrail:us-east-1:123456789012:trail/MyTrail@
stopLogging
    :: Text -- ^ 'slName'
    -> StopLogging
stopLogging pName_ = StopLogging' {_slName = pName_}


-- | Specifies the name or the CloudTrail ARN of the trail for which CloudTrail will stop logging AWS API calls. The format of a trail ARN is: @arn:aws:cloudtrail:us-east-1:123456789012:trail/MyTrail@
slName :: Lens' StopLogging Text
slName = lens _slName (\ s a -> s{_slName = a})

instance AWSRequest StopLogging where
        type Rs StopLogging = StopLoggingResponse
        request = postJSON cloudTrail
        response
          = receiveEmpty
              (\ s h x ->
                 StopLoggingResponse' <$> (pure (fromEnum s)))

instance Hashable StopLogging where

instance NFData StopLogging where

instance ToHeaders StopLogging where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.StopLogging"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StopLogging where
        toJSON StopLogging'{..}
          = object (catMaybes [Just ("Name" .= _slName)])

instance ToPath StopLogging where
        toPath = const "/"

instance ToQuery StopLogging where
        toQuery = const mempty

-- | Returns the objects or data listed below if successful. Otherwise, returns an error.
--
--
--
-- /See:/ 'stopLoggingResponse' smart constructor.
newtype StopLoggingResponse = StopLoggingResponse'
  { _slrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopLoggingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slrsResponseStatus' - -- | The response status code.
stopLoggingResponse
    :: Int -- ^ 'slrsResponseStatus'
    -> StopLoggingResponse
stopLoggingResponse pResponseStatus_ =
  StopLoggingResponse' {_slrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
slrsResponseStatus :: Lens' StopLoggingResponse Int
slrsResponseStatus = lens _slrsResponseStatus (\ s a -> s{_slrsResponseStatus = a})

instance NFData StopLoggingResponse where
