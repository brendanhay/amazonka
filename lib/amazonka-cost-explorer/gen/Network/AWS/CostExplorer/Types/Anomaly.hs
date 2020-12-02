{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.Anomaly
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.Anomaly where

import Network.AWS.CostExplorer.Types.AnomalyFeedbackType
import Network.AWS.CostExplorer.Types.AnomalyScore
import Network.AWS.CostExplorer.Types.Impact
import Network.AWS.CostExplorer.Types.RootCause
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An unusual cost pattern. This consists of the detailed metadata and the current status of the anomaly object.
--
--
--
-- /See:/ 'anomaly' smart constructor.
data Anomaly = Anomaly'
  { _aAnomalyStartDate :: !(Maybe Text),
    _aDimensionValue :: !(Maybe Text),
    _aRootCauses :: !(Maybe [RootCause]),
    _aAnomalyEndDate :: !(Maybe Text),
    _aFeedback :: !(Maybe AnomalyFeedbackType),
    _aAnomalyId :: !Text,
    _aAnomalyScore :: !AnomalyScore,
    _aImpact :: !Impact,
    _aMonitorARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Anomaly' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aAnomalyStartDate' - The first day the anomaly is detected.
--
-- * 'aDimensionValue' - The dimension for the anomaly. For example, an AWS service in a service monitor.
--
-- * 'aRootCauses' - The list of identified root causes for the anomaly.
--
-- * 'aAnomalyEndDate' - The last day the anomaly is detected.
--
-- * 'aFeedback' - The feedback value.
--
-- * 'aAnomalyId' - The unique identifier for the anomaly.
--
-- * 'aAnomalyScore' - The latest and maximum score for the anomaly.
--
-- * 'aImpact' - The dollar impact for the anomaly.
--
-- * 'aMonitorARN' - The Amazon Resource Name (ARN) for the cost monitor that generated this anomaly.
anomaly ::
  -- | 'aAnomalyId'
  Text ->
  -- | 'aAnomalyScore'
  AnomalyScore ->
  -- | 'aImpact'
  Impact ->
  -- | 'aMonitorARN'
  Text ->
  Anomaly
anomaly pAnomalyId_ pAnomalyScore_ pImpact_ pMonitorARN_ =
  Anomaly'
    { _aAnomalyStartDate = Nothing,
      _aDimensionValue = Nothing,
      _aRootCauses = Nothing,
      _aAnomalyEndDate = Nothing,
      _aFeedback = Nothing,
      _aAnomalyId = pAnomalyId_,
      _aAnomalyScore = pAnomalyScore_,
      _aImpact = pImpact_,
      _aMonitorARN = pMonitorARN_
    }

-- | The first day the anomaly is detected.
aAnomalyStartDate :: Lens' Anomaly (Maybe Text)
aAnomalyStartDate = lens _aAnomalyStartDate (\s a -> s {_aAnomalyStartDate = a})

-- | The dimension for the anomaly. For example, an AWS service in a service monitor.
aDimensionValue :: Lens' Anomaly (Maybe Text)
aDimensionValue = lens _aDimensionValue (\s a -> s {_aDimensionValue = a})

-- | The list of identified root causes for the anomaly.
aRootCauses :: Lens' Anomaly [RootCause]
aRootCauses = lens _aRootCauses (\s a -> s {_aRootCauses = a}) . _Default . _Coerce

-- | The last day the anomaly is detected.
aAnomalyEndDate :: Lens' Anomaly (Maybe Text)
aAnomalyEndDate = lens _aAnomalyEndDate (\s a -> s {_aAnomalyEndDate = a})

-- | The feedback value.
aFeedback :: Lens' Anomaly (Maybe AnomalyFeedbackType)
aFeedback = lens _aFeedback (\s a -> s {_aFeedback = a})

-- | The unique identifier for the anomaly.
aAnomalyId :: Lens' Anomaly Text
aAnomalyId = lens _aAnomalyId (\s a -> s {_aAnomalyId = a})

-- | The latest and maximum score for the anomaly.
aAnomalyScore :: Lens' Anomaly AnomalyScore
aAnomalyScore = lens _aAnomalyScore (\s a -> s {_aAnomalyScore = a})

-- | The dollar impact for the anomaly.
aImpact :: Lens' Anomaly Impact
aImpact = lens _aImpact (\s a -> s {_aImpact = a})

-- | The Amazon Resource Name (ARN) for the cost monitor that generated this anomaly.
aMonitorARN :: Lens' Anomaly Text
aMonitorARN = lens _aMonitorARN (\s a -> s {_aMonitorARN = a})

instance FromJSON Anomaly where
  parseJSON =
    withObject
      "Anomaly"
      ( \x ->
          Anomaly'
            <$> (x .:? "AnomalyStartDate")
            <*> (x .:? "DimensionValue")
            <*> (x .:? "RootCauses" .!= mempty)
            <*> (x .:? "AnomalyEndDate")
            <*> (x .:? "Feedback")
            <*> (x .: "AnomalyId")
            <*> (x .: "AnomalyScore")
            <*> (x .: "Impact")
            <*> (x .: "MonitorArn")
      )

instance Hashable Anomaly

instance NFData Anomaly
