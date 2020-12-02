{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.UpdateGatewayInformation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a gateway's metadata, which includes the gateway's name and time zone. To specify which gateway to update, use the Amazon Resource Name (ARN) of the gateway in your request.
module Network.AWS.StorageGateway.UpdateGatewayInformation
  ( -- * Creating a Request
    updateGatewayInformation,
    UpdateGatewayInformation,

    -- * Request Lenses
    ugiGatewayName,
    ugiGatewayTimezone,
    ugiCloudWatchLogGroupARN,
    ugiGatewayARN,

    -- * Destructuring the Response
    updateGatewayInformationResponse,
    UpdateGatewayInformationResponse,

    -- * Response Lenses
    ugirsGatewayARN,
    ugirsGatewayName,
    ugirsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types

-- | /See:/ 'updateGatewayInformation' smart constructor.
data UpdateGatewayInformation = UpdateGatewayInformation'
  { _ugiGatewayName ::
      !(Maybe Text),
    _ugiGatewayTimezone :: !(Maybe Text),
    _ugiCloudWatchLogGroupARN ::
      !(Maybe Text),
    _ugiGatewayARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateGatewayInformation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ugiGatewayName' - Undocumented member.
--
-- * 'ugiGatewayTimezone' - A value that indicates the time zone of the gateway.
--
-- * 'ugiCloudWatchLogGroupARN' - The Amazon Resource Name (ARN) of the Amazon CloudWatch log group that you want to use to monitor and log events in the gateway. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/WhatIsCloudWatchLogs.html What is Amazon CloudWatch Logs?>
--
-- * 'ugiGatewayARN' - Undocumented member.
updateGatewayInformation ::
  -- | 'ugiGatewayARN'
  Text ->
  UpdateGatewayInformation
updateGatewayInformation pGatewayARN_ =
  UpdateGatewayInformation'
    { _ugiGatewayName = Nothing,
      _ugiGatewayTimezone = Nothing,
      _ugiCloudWatchLogGroupARN = Nothing,
      _ugiGatewayARN = pGatewayARN_
    }

-- | Undocumented member.
ugiGatewayName :: Lens' UpdateGatewayInformation (Maybe Text)
ugiGatewayName = lens _ugiGatewayName (\s a -> s {_ugiGatewayName = a})

-- | A value that indicates the time zone of the gateway.
ugiGatewayTimezone :: Lens' UpdateGatewayInformation (Maybe Text)
ugiGatewayTimezone = lens _ugiGatewayTimezone (\s a -> s {_ugiGatewayTimezone = a})

-- | The Amazon Resource Name (ARN) of the Amazon CloudWatch log group that you want to use to monitor and log events in the gateway. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/WhatIsCloudWatchLogs.html What is Amazon CloudWatch Logs?>
ugiCloudWatchLogGroupARN :: Lens' UpdateGatewayInformation (Maybe Text)
ugiCloudWatchLogGroupARN = lens _ugiCloudWatchLogGroupARN (\s a -> s {_ugiCloudWatchLogGroupARN = a})

-- | Undocumented member.
ugiGatewayARN :: Lens' UpdateGatewayInformation Text
ugiGatewayARN = lens _ugiGatewayARN (\s a -> s {_ugiGatewayARN = a})

instance AWSRequest UpdateGatewayInformation where
  type Rs UpdateGatewayInformation = UpdateGatewayInformationResponse
  request = postJSON storageGateway
  response =
    receiveJSON
      ( \s h x ->
          UpdateGatewayInformationResponse'
            <$> (x .?> "GatewayARN")
            <*> (x .?> "GatewayName")
            <*> (pure (fromEnum s))
      )

instance Hashable UpdateGatewayInformation

instance NFData UpdateGatewayInformation

instance ToHeaders UpdateGatewayInformation where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("StorageGateway_20130630.UpdateGatewayInformation" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateGatewayInformation where
  toJSON UpdateGatewayInformation' {..} =
    object
      ( catMaybes
          [ ("GatewayName" .=) <$> _ugiGatewayName,
            ("GatewayTimezone" .=) <$> _ugiGatewayTimezone,
            ("CloudWatchLogGroupARN" .=) <$> _ugiCloudWatchLogGroupARN,
            Just ("GatewayARN" .= _ugiGatewayARN)
          ]
      )

instance ToPath UpdateGatewayInformation where
  toPath = const "/"

instance ToQuery UpdateGatewayInformation where
  toQuery = const mempty

-- | A JSON object containing the Amazon Resource Name (ARN) of the gateway that was updated.
--
--
--
-- /See:/ 'updateGatewayInformationResponse' smart constructor.
data UpdateGatewayInformationResponse = UpdateGatewayInformationResponse'
  { _ugirsGatewayARN ::
      !(Maybe Text),
    _ugirsGatewayName ::
      !(Maybe Text),
    _ugirsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateGatewayInformationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ugirsGatewayARN' - Undocumented member.
--
-- * 'ugirsGatewayName' - The name you configured for your gateway.
--
-- * 'ugirsResponseStatus' - -- | The response status code.
updateGatewayInformationResponse ::
  -- | 'ugirsResponseStatus'
  Int ->
  UpdateGatewayInformationResponse
updateGatewayInformationResponse pResponseStatus_ =
  UpdateGatewayInformationResponse'
    { _ugirsGatewayARN = Nothing,
      _ugirsGatewayName = Nothing,
      _ugirsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
ugirsGatewayARN :: Lens' UpdateGatewayInformationResponse (Maybe Text)
ugirsGatewayARN = lens _ugirsGatewayARN (\s a -> s {_ugirsGatewayARN = a})

-- | The name you configured for your gateway.
ugirsGatewayName :: Lens' UpdateGatewayInformationResponse (Maybe Text)
ugirsGatewayName = lens _ugirsGatewayName (\s a -> s {_ugirsGatewayName = a})

-- | -- | The response status code.
ugirsResponseStatus :: Lens' UpdateGatewayInformationResponse Int
ugirsResponseStatus = lens _ugirsResponseStatus (\s a -> s {_ugirsResponseStatus = a})

instance NFData UpdateGatewayInformationResponse
