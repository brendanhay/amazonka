{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.AnomalySubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.AnomalySubscription where

import Network.AWS.CostExplorer.Types.AnomalySubscriptionFrequency
import Network.AWS.CostExplorer.Types.Subscriber
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The association between a monitor, threshold, and list of subscribers used to deliver notifications about anomalies detected by a monitor that exceeds a threshold. The content consists of the detailed metadata and the current status of the @AnomalySubscription@ object.
--
--
--
-- /See:/ 'anomalySubscription' smart constructor.
data AnomalySubscription = AnomalySubscription'
  { _asAccountId ::
      !(Maybe Text),
    _asSubscriptionARN :: !(Maybe Text),
    _asMonitorARNList :: ![Text],
    _asSubscribers :: ![Subscriber],
    _asThreshold :: !Double,
    _asFrequency :: !AnomalySubscriptionFrequency,
    _asSubscriptionName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AnomalySubscription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asAccountId' - Your unique account identifier.
--
-- * 'asSubscriptionARN' - The @AnomalySubscription@ Amazon Resource Name (ARN).
--
-- * 'asMonitorARNList' - A list of cost anomaly monitors.
--
-- * 'asSubscribers' - A list of subscribers to notify.
--
-- * 'asThreshold' - The dollar value that triggers a notification if the threshold is exceeded.
--
-- * 'asFrequency' - The frequency at which anomaly reports are sent over email.
--
-- * 'asSubscriptionName' - The name for the subscription.
anomalySubscription ::
  -- | 'asThreshold'
  Double ->
  -- | 'asFrequency'
  AnomalySubscriptionFrequency ->
  -- | 'asSubscriptionName'
  Text ->
  AnomalySubscription
anomalySubscription pThreshold_ pFrequency_ pSubscriptionName_ =
  AnomalySubscription'
    { _asAccountId = Nothing,
      _asSubscriptionARN = Nothing,
      _asMonitorARNList = mempty,
      _asSubscribers = mempty,
      _asThreshold = pThreshold_,
      _asFrequency = pFrequency_,
      _asSubscriptionName = pSubscriptionName_
    }

-- | Your unique account identifier.
asAccountId :: Lens' AnomalySubscription (Maybe Text)
asAccountId = lens _asAccountId (\s a -> s {_asAccountId = a})

-- | The @AnomalySubscription@ Amazon Resource Name (ARN).
asSubscriptionARN :: Lens' AnomalySubscription (Maybe Text)
asSubscriptionARN = lens _asSubscriptionARN (\s a -> s {_asSubscriptionARN = a})

-- | A list of cost anomaly monitors.
asMonitorARNList :: Lens' AnomalySubscription [Text]
asMonitorARNList = lens _asMonitorARNList (\s a -> s {_asMonitorARNList = a}) . _Coerce

-- | A list of subscribers to notify.
asSubscribers :: Lens' AnomalySubscription [Subscriber]
asSubscribers = lens _asSubscribers (\s a -> s {_asSubscribers = a}) . _Coerce

-- | The dollar value that triggers a notification if the threshold is exceeded.
asThreshold :: Lens' AnomalySubscription Double
asThreshold = lens _asThreshold (\s a -> s {_asThreshold = a})

-- | The frequency at which anomaly reports are sent over email.
asFrequency :: Lens' AnomalySubscription AnomalySubscriptionFrequency
asFrequency = lens _asFrequency (\s a -> s {_asFrequency = a})

-- | The name for the subscription.
asSubscriptionName :: Lens' AnomalySubscription Text
asSubscriptionName = lens _asSubscriptionName (\s a -> s {_asSubscriptionName = a})

instance FromJSON AnomalySubscription where
  parseJSON =
    withObject
      "AnomalySubscription"
      ( \x ->
          AnomalySubscription'
            <$> (x .:? "AccountId")
            <*> (x .:? "SubscriptionArn")
            <*> (x .:? "MonitorArnList" .!= mempty)
            <*> (x .:? "Subscribers" .!= mempty)
            <*> (x .: "Threshold")
            <*> (x .: "Frequency")
            <*> (x .: "SubscriptionName")
      )

instance Hashable AnomalySubscription

instance NFData AnomalySubscription

instance ToJSON AnomalySubscription where
  toJSON AnomalySubscription' {..} =
    object
      ( catMaybes
          [ ("AccountId" .=) <$> _asAccountId,
            ("SubscriptionArn" .=) <$> _asSubscriptionARN,
            Just ("MonitorArnList" .= _asMonitorARNList),
            Just ("Subscribers" .= _asSubscribers),
            Just ("Threshold" .= _asThreshold),
            Just ("Frequency" .= _asFrequency),
            Just ("SubscriptionName" .= _asSubscriptionName)
          ]
      )
