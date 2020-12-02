{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.AnomalyMonitor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.AnomalyMonitor where

import Network.AWS.CostExplorer.Types.Expression
import Network.AWS.CostExplorer.Types.MonitorDimension
import Network.AWS.CostExplorer.Types.MonitorType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | This object continuously inspects your account's cost data for anomalies, based on @MonitorType@ and @MonitorSpecification@ . The content consists of detailed metadata and the current status of the monitor object.
--
--
--
-- /See:/ 'anomalyMonitor' smart constructor.
data AnomalyMonitor = AnomalyMonitor'
  { _amDimensionalValueCount ::
      !(Maybe Nat),
    _amMonitorSpecification :: !(Maybe Expression),
    _amMonitorDimension :: !(Maybe MonitorDimension),
    _amCreationDate :: !(Maybe Text),
    _amLastUpdatedDate :: !(Maybe Text),
    _amLastEvaluatedDate :: !(Maybe Text),
    _amMonitorARN :: !(Maybe Text),
    _amMonitorName :: !Text,
    _amMonitorType :: !MonitorType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AnomalyMonitor' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'amDimensionalValueCount' - The value for evaluated dimensions.
--
-- * 'amMonitorSpecification' - Undocumented member.
--
-- * 'amMonitorDimension' - The dimensions to evaluate.
--
-- * 'amCreationDate' - The date when the monitor was created.
--
-- * 'amLastUpdatedDate' - The date when the monitor was last updated.
--
-- * 'amLastEvaluatedDate' - The date when the monitor last evaluated for anomalies.
--
-- * 'amMonitorARN' - The Amazon Resource Name (ARN) value.
--
-- * 'amMonitorName' - The name of the monitor.
--
-- * 'amMonitorType' - The possible type values.
anomalyMonitor ::
  -- | 'amMonitorName'
  Text ->
  -- | 'amMonitorType'
  MonitorType ->
  AnomalyMonitor
anomalyMonitor pMonitorName_ pMonitorType_ =
  AnomalyMonitor'
    { _amDimensionalValueCount = Nothing,
      _amMonitorSpecification = Nothing,
      _amMonitorDimension = Nothing,
      _amCreationDate = Nothing,
      _amLastUpdatedDate = Nothing,
      _amLastEvaluatedDate = Nothing,
      _amMonitorARN = Nothing,
      _amMonitorName = pMonitorName_,
      _amMonitorType = pMonitorType_
    }

-- | The value for evaluated dimensions.
amDimensionalValueCount :: Lens' AnomalyMonitor (Maybe Natural)
amDimensionalValueCount = lens _amDimensionalValueCount (\s a -> s {_amDimensionalValueCount = a}) . mapping _Nat

-- | Undocumented member.
amMonitorSpecification :: Lens' AnomalyMonitor (Maybe Expression)
amMonitorSpecification = lens _amMonitorSpecification (\s a -> s {_amMonitorSpecification = a})

-- | The dimensions to evaluate.
amMonitorDimension :: Lens' AnomalyMonitor (Maybe MonitorDimension)
amMonitorDimension = lens _amMonitorDimension (\s a -> s {_amMonitorDimension = a})

-- | The date when the monitor was created.
amCreationDate :: Lens' AnomalyMonitor (Maybe Text)
amCreationDate = lens _amCreationDate (\s a -> s {_amCreationDate = a})

-- | The date when the monitor was last updated.
amLastUpdatedDate :: Lens' AnomalyMonitor (Maybe Text)
amLastUpdatedDate = lens _amLastUpdatedDate (\s a -> s {_amLastUpdatedDate = a})

-- | The date when the monitor last evaluated for anomalies.
amLastEvaluatedDate :: Lens' AnomalyMonitor (Maybe Text)
amLastEvaluatedDate = lens _amLastEvaluatedDate (\s a -> s {_amLastEvaluatedDate = a})

-- | The Amazon Resource Name (ARN) value.
amMonitorARN :: Lens' AnomalyMonitor (Maybe Text)
amMonitorARN = lens _amMonitorARN (\s a -> s {_amMonitorARN = a})

-- | The name of the monitor.
amMonitorName :: Lens' AnomalyMonitor Text
amMonitorName = lens _amMonitorName (\s a -> s {_amMonitorName = a})

-- | The possible type values.
amMonitorType :: Lens' AnomalyMonitor MonitorType
amMonitorType = lens _amMonitorType (\s a -> s {_amMonitorType = a})

instance FromJSON AnomalyMonitor where
  parseJSON =
    withObject
      "AnomalyMonitor"
      ( \x ->
          AnomalyMonitor'
            <$> (x .:? "DimensionalValueCount")
            <*> (x .:? "MonitorSpecification")
            <*> (x .:? "MonitorDimension")
            <*> (x .:? "CreationDate")
            <*> (x .:? "LastUpdatedDate")
            <*> (x .:? "LastEvaluatedDate")
            <*> (x .:? "MonitorArn")
            <*> (x .: "MonitorName")
            <*> (x .: "MonitorType")
      )

instance Hashable AnomalyMonitor

instance NFData AnomalyMonitor

instance ToJSON AnomalyMonitor where
  toJSON AnomalyMonitor' {..} =
    object
      ( catMaybes
          [ ("DimensionalValueCount" .=) <$> _amDimensionalValueCount,
            ("MonitorSpecification" .=) <$> _amMonitorSpecification,
            ("MonitorDimension" .=) <$> _amMonitorDimension,
            ("CreationDate" .=) <$> _amCreationDate,
            ("LastUpdatedDate" .=) <$> _amLastUpdatedDate,
            ("LastEvaluatedDate" .=) <$> _amLastEvaluatedDate,
            ("MonitorArn" .=) <$> _amMonitorARN,
            Just ("MonitorName" .= _amMonitorName),
            Just ("MonitorType" .= _amMonitorType)
          ]
      )
