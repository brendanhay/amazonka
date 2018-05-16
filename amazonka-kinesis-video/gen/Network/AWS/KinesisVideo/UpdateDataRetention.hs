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
-- Module      : Network.AWS.KinesisVideo.UpdateDataRetention
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Increases or decreases the stream's data retention period by the value that you specify. To indicate whether you want to increase or decrease the data retention period, specify the @Operation@ parameter in the request body. In the request, you must specify either the @StreamName@ or the @StreamARN@ .
--
--
-- This operation requires permission for the @KinesisVideo:UpdateDataRetention@ action.
--
-- Changing the data retention period affects the data in the stream as follows:
--
--     * If the data retention period is increased, existing data is retained for the new retention period. For example, if the data retention period is increased from one hour to seven hours, all existing data is retained for seven hours.
--
--     * If the data retention period is decreased, existing data is retained for the new retention period. For example, if the data retention period is decreased from seven hours to one hour, all existing data is retained for one hour, and any data older than one hour is deleted immediately.
--
--
--
module Network.AWS.KinesisVideo.UpdateDataRetention
    (
    -- * Creating a Request
      updateDataRetention
    , UpdateDataRetention
    -- * Request Lenses
    , udrStreamARN
    , udrStreamName
    , udrCurrentVersion
    , udrOperation
    , udrDataRetentionChangeInHours

    -- * Destructuring the Response
    , updateDataRetentionResponse
    , UpdateDataRetentionResponse
    -- * Response Lenses
    , udrrsResponseStatus
    ) where

import Network.AWS.KinesisVideo.Types
import Network.AWS.KinesisVideo.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateDataRetention' smart constructor.
data UpdateDataRetention = UpdateDataRetention'
  { _udrStreamARN                  :: !(Maybe Text)
  , _udrStreamName                 :: !(Maybe Text)
  , _udrCurrentVersion             :: !Text
  , _udrOperation                  :: !UpdateDataRetentionOperation
  , _udrDataRetentionChangeInHours :: !Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateDataRetention' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udrStreamARN' - The Amazon Resource Name (ARN) of the stream whose retention period you want to change.
--
-- * 'udrStreamName' - The name of the stream whose retention period you want to change.
--
-- * 'udrCurrentVersion' - The version of the stream whose retention period you want to change. To get the version, call either the @DescribeStream@ or the @ListStreams@ API.
--
-- * 'udrOperation' - Indicates whether you want to increase or decrease the retention period.
--
-- * 'udrDataRetentionChangeInHours' - The retention period, in hours. The value you specify replaces the current value.
updateDataRetention
    :: Text -- ^ 'udrCurrentVersion'
    -> UpdateDataRetentionOperation -- ^ 'udrOperation'
    -> Natural -- ^ 'udrDataRetentionChangeInHours'
    -> UpdateDataRetention
updateDataRetention pCurrentVersion_ pOperation_ pDataRetentionChangeInHours_ =
  UpdateDataRetention'
    { _udrStreamARN = Nothing
    , _udrStreamName = Nothing
    , _udrCurrentVersion = pCurrentVersion_
    , _udrOperation = pOperation_
    , _udrDataRetentionChangeInHours = _Nat # pDataRetentionChangeInHours_
    }


-- | The Amazon Resource Name (ARN) of the stream whose retention period you want to change.
udrStreamARN :: Lens' UpdateDataRetention (Maybe Text)
udrStreamARN = lens _udrStreamARN (\ s a -> s{_udrStreamARN = a})

-- | The name of the stream whose retention period you want to change.
udrStreamName :: Lens' UpdateDataRetention (Maybe Text)
udrStreamName = lens _udrStreamName (\ s a -> s{_udrStreamName = a})

-- | The version of the stream whose retention period you want to change. To get the version, call either the @DescribeStream@ or the @ListStreams@ API.
udrCurrentVersion :: Lens' UpdateDataRetention Text
udrCurrentVersion = lens _udrCurrentVersion (\ s a -> s{_udrCurrentVersion = a})

-- | Indicates whether you want to increase or decrease the retention period.
udrOperation :: Lens' UpdateDataRetention UpdateDataRetentionOperation
udrOperation = lens _udrOperation (\ s a -> s{_udrOperation = a})

-- | The retention period, in hours. The value you specify replaces the current value.
udrDataRetentionChangeInHours :: Lens' UpdateDataRetention Natural
udrDataRetentionChangeInHours = lens _udrDataRetentionChangeInHours (\ s a -> s{_udrDataRetentionChangeInHours = a}) . _Nat

instance AWSRequest UpdateDataRetention where
        type Rs UpdateDataRetention =
             UpdateDataRetentionResponse
        request = postJSON kinesisVideo
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateDataRetentionResponse' <$> (pure (fromEnum s)))

instance Hashable UpdateDataRetention where

instance NFData UpdateDataRetention where

instance ToHeaders UpdateDataRetention where
        toHeaders = const mempty

instance ToJSON UpdateDataRetention where
        toJSON UpdateDataRetention'{..}
          = object
              (catMaybes
                 [("StreamARN" .=) <$> _udrStreamARN,
                  ("StreamName" .=) <$> _udrStreamName,
                  Just ("CurrentVersion" .= _udrCurrentVersion),
                  Just ("Operation" .= _udrOperation),
                  Just
                    ("DataRetentionChangeInHours" .=
                       _udrDataRetentionChangeInHours)])

instance ToPath UpdateDataRetention where
        toPath = const "/updateDataRetention"

instance ToQuery UpdateDataRetention where
        toQuery = const mempty

-- | /See:/ 'updateDataRetentionResponse' smart constructor.
newtype UpdateDataRetentionResponse = UpdateDataRetentionResponse'
  { _udrrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateDataRetentionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udrrsResponseStatus' - -- | The response status code.
updateDataRetentionResponse
    :: Int -- ^ 'udrrsResponseStatus'
    -> UpdateDataRetentionResponse
updateDataRetentionResponse pResponseStatus_ =
  UpdateDataRetentionResponse' {_udrrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
udrrsResponseStatus :: Lens' UpdateDataRetentionResponse Int
udrrsResponseStatus = lens _udrrsResponseStatus (\ s a -> s{_udrrsResponseStatus = a})

instance NFData UpdateDataRetentionResponse where
