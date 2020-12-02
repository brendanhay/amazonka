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
-- Module      : Network.AWS.CostExplorer.UpdateAnomalySubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing cost anomaly monitor subscription.
module Network.AWS.CostExplorer.UpdateAnomalySubscription
  ( -- * Creating a Request
    updateAnomalySubscription,
    UpdateAnomalySubscription,

    -- * Request Lenses
    uasSubscriptionName,
    uasFrequency,
    uasThreshold,
    uasMonitorARNList,
    uasSubscribers,
    uasSubscriptionARN,

    -- * Destructuring the Response
    updateAnomalySubscriptionResponse,
    UpdateAnomalySubscriptionResponse,

    -- * Response Lenses
    uasrsResponseStatus,
    uasrsSubscriptionARN,
  )
where

import Network.AWS.CostExplorer.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateAnomalySubscription' smart constructor.
data UpdateAnomalySubscription = UpdateAnomalySubscription'
  { _uasSubscriptionName ::
      !(Maybe Text),
    _uasFrequency ::
      !(Maybe AnomalySubscriptionFrequency),
    _uasThreshold :: !(Maybe Double),
    _uasMonitorARNList :: !(Maybe [Text]),
    _uasSubscribers ::
      !(Maybe [Subscriber]),
    _uasSubscriptionARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateAnomalySubscription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uasSubscriptionName' - The subscription's new name.
--
-- * 'uasFrequency' - The update to the frequency value at which subscribers will receive notifications.
--
-- * 'uasThreshold' - The update to the threshold value for receiving notifications.
--
-- * 'uasMonitorARNList' - A list of cost anomaly subscription ARNs.
--
-- * 'uasSubscribers' - The update to the subscriber list.
--
-- * 'uasSubscriptionARN' - A cost anomaly subscription Amazon Resource Name (ARN).
updateAnomalySubscription ::
  -- | 'uasSubscriptionARN'
  Text ->
  UpdateAnomalySubscription
updateAnomalySubscription pSubscriptionARN_ =
  UpdateAnomalySubscription'
    { _uasSubscriptionName = Nothing,
      _uasFrequency = Nothing,
      _uasThreshold = Nothing,
      _uasMonitorARNList = Nothing,
      _uasSubscribers = Nothing,
      _uasSubscriptionARN = pSubscriptionARN_
    }

-- | The subscription's new name.
uasSubscriptionName :: Lens' UpdateAnomalySubscription (Maybe Text)
uasSubscriptionName = lens _uasSubscriptionName (\s a -> s {_uasSubscriptionName = a})

-- | The update to the frequency value at which subscribers will receive notifications.
uasFrequency :: Lens' UpdateAnomalySubscription (Maybe AnomalySubscriptionFrequency)
uasFrequency = lens _uasFrequency (\s a -> s {_uasFrequency = a})

-- | The update to the threshold value for receiving notifications.
uasThreshold :: Lens' UpdateAnomalySubscription (Maybe Double)
uasThreshold = lens _uasThreshold (\s a -> s {_uasThreshold = a})

-- | A list of cost anomaly subscription ARNs.
uasMonitorARNList :: Lens' UpdateAnomalySubscription [Text]
uasMonitorARNList = lens _uasMonitorARNList (\s a -> s {_uasMonitorARNList = a}) . _Default . _Coerce

-- | The update to the subscriber list.
uasSubscribers :: Lens' UpdateAnomalySubscription [Subscriber]
uasSubscribers = lens _uasSubscribers (\s a -> s {_uasSubscribers = a}) . _Default . _Coerce

-- | A cost anomaly subscription Amazon Resource Name (ARN).
uasSubscriptionARN :: Lens' UpdateAnomalySubscription Text
uasSubscriptionARN = lens _uasSubscriptionARN (\s a -> s {_uasSubscriptionARN = a})

instance AWSRequest UpdateAnomalySubscription where
  type
    Rs UpdateAnomalySubscription =
      UpdateAnomalySubscriptionResponse
  request = postJSON costExplorer
  response =
    receiveJSON
      ( \s h x ->
          UpdateAnomalySubscriptionResponse'
            <$> (pure (fromEnum s)) <*> (x .:> "SubscriptionArn")
      )

instance Hashable UpdateAnomalySubscription

instance NFData UpdateAnomalySubscription

instance ToHeaders UpdateAnomalySubscription where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSInsightsIndexService.UpdateAnomalySubscription" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateAnomalySubscription where
  toJSON UpdateAnomalySubscription' {..} =
    object
      ( catMaybes
          [ ("SubscriptionName" .=) <$> _uasSubscriptionName,
            ("Frequency" .=) <$> _uasFrequency,
            ("Threshold" .=) <$> _uasThreshold,
            ("MonitorArnList" .=) <$> _uasMonitorARNList,
            ("Subscribers" .=) <$> _uasSubscribers,
            Just ("SubscriptionArn" .= _uasSubscriptionARN)
          ]
      )

instance ToPath UpdateAnomalySubscription where
  toPath = const "/"

instance ToQuery UpdateAnomalySubscription where
  toQuery = const mempty

-- | /See:/ 'updateAnomalySubscriptionResponse' smart constructor.
data UpdateAnomalySubscriptionResponse = UpdateAnomalySubscriptionResponse'
  { _uasrsResponseStatus ::
      !Int,
    _uasrsSubscriptionARN ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateAnomalySubscriptionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uasrsResponseStatus' - -- | The response status code.
--
-- * 'uasrsSubscriptionARN' - A cost anomaly subscription ARN.
updateAnomalySubscriptionResponse ::
  -- | 'uasrsResponseStatus'
  Int ->
  -- | 'uasrsSubscriptionARN'
  Text ->
  UpdateAnomalySubscriptionResponse
updateAnomalySubscriptionResponse
  pResponseStatus_
  pSubscriptionARN_ =
    UpdateAnomalySubscriptionResponse'
      { _uasrsResponseStatus =
          pResponseStatus_,
        _uasrsSubscriptionARN = pSubscriptionARN_
      }

-- | -- | The response status code.
uasrsResponseStatus :: Lens' UpdateAnomalySubscriptionResponse Int
uasrsResponseStatus = lens _uasrsResponseStatus (\s a -> s {_uasrsResponseStatus = a})

-- | A cost anomaly subscription ARN.
uasrsSubscriptionARN :: Lens' UpdateAnomalySubscriptionResponse Text
uasrsSubscriptionARN = lens _uasrsSubscriptionARN (\s a -> s {_uasrsSubscriptionARN = a})

instance NFData UpdateAnomalySubscriptionResponse
