{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.Insight
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.Insight where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.XRay.Types.AnomalousService
import Network.AWS.XRay.Types.InsightCategory
import Network.AWS.XRay.Types.InsightState
import Network.AWS.XRay.Types.RequestImpactStatistics
import Network.AWS.XRay.Types.ServiceId

-- | When fault rates go outside of the expected range, X-Ray creates an insight. Insights tracks emergent issues within your applications.
--
--
--
-- /See:/ 'insight' smart constructor.
data Insight = Insight'
  { _iSummary :: !(Maybe Text),
    _iState :: !(Maybe InsightState),
    _iStartTime :: !(Maybe POSIX),
    _iInsightId :: !(Maybe Text),
    _iCategories :: !(Maybe [InsightCategory]),
    _iRootCauseServiceRequestImpactStatistics ::
      !(Maybe RequestImpactStatistics),
    _iTopAnomalousServices :: !(Maybe [AnomalousService]),
    _iRootCauseServiceId :: !(Maybe ServiceId),
    _iClientRequestImpactStatistics ::
      !(Maybe RequestImpactStatistics),
    _iEndTime :: !(Maybe POSIX),
    _iGroupARN :: !(Maybe Text),
    _iGroupName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Insight' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iSummary' - A brief description of the insight.
--
-- * 'iState' - The current state of the insight.
--
-- * 'iStartTime' - The time, in Unix seconds, at which the insight began.
--
-- * 'iInsightId' - The insights unique identifier.
--
-- * 'iCategories' - The categories that label and describe the type of insight.
--
-- * 'iRootCauseServiceRequestImpactStatistics' - The impact statistics of the root cause service. This includes the number of requests to the client service and whether the requests were faults or okay.
--
-- * 'iTopAnomalousServices' - The service within the insight that is most impacted by the incident.
--
-- * 'iRootCauseServiceId' - Undocumented member.
--
-- * 'iClientRequestImpactStatistics' - The impact statistics of the client side service. This includes the number of requests to the client service and whether the requests were faults or okay.
--
-- * 'iEndTime' - The time, in Unix seconds, at which the insight ended.
--
-- * 'iGroupARN' - The Amazon Resource Name (ARN) of the group that the insight belongs to.
--
-- * 'iGroupName' - The name of the group that the insight belongs to.
insight ::
  Insight
insight =
  Insight'
    { _iSummary = Nothing,
      _iState = Nothing,
      _iStartTime = Nothing,
      _iInsightId = Nothing,
      _iCategories = Nothing,
      _iRootCauseServiceRequestImpactStatistics = Nothing,
      _iTopAnomalousServices = Nothing,
      _iRootCauseServiceId = Nothing,
      _iClientRequestImpactStatistics = Nothing,
      _iEndTime = Nothing,
      _iGroupARN = Nothing,
      _iGroupName = Nothing
    }

-- | A brief description of the insight.
iSummary :: Lens' Insight (Maybe Text)
iSummary = lens _iSummary (\s a -> s {_iSummary = a})

-- | The current state of the insight.
iState :: Lens' Insight (Maybe InsightState)
iState = lens _iState (\s a -> s {_iState = a})

-- | The time, in Unix seconds, at which the insight began.
iStartTime :: Lens' Insight (Maybe UTCTime)
iStartTime = lens _iStartTime (\s a -> s {_iStartTime = a}) . mapping _Time

-- | The insights unique identifier.
iInsightId :: Lens' Insight (Maybe Text)
iInsightId = lens _iInsightId (\s a -> s {_iInsightId = a})

-- | The categories that label and describe the type of insight.
iCategories :: Lens' Insight [InsightCategory]
iCategories = lens _iCategories (\s a -> s {_iCategories = a}) . _Default . _Coerce

-- | The impact statistics of the root cause service. This includes the number of requests to the client service and whether the requests were faults or okay.
iRootCauseServiceRequestImpactStatistics :: Lens' Insight (Maybe RequestImpactStatistics)
iRootCauseServiceRequestImpactStatistics = lens _iRootCauseServiceRequestImpactStatistics (\s a -> s {_iRootCauseServiceRequestImpactStatistics = a})

-- | The service within the insight that is most impacted by the incident.
iTopAnomalousServices :: Lens' Insight [AnomalousService]
iTopAnomalousServices = lens _iTopAnomalousServices (\s a -> s {_iTopAnomalousServices = a}) . _Default . _Coerce

-- | Undocumented member.
iRootCauseServiceId :: Lens' Insight (Maybe ServiceId)
iRootCauseServiceId = lens _iRootCauseServiceId (\s a -> s {_iRootCauseServiceId = a})

-- | The impact statistics of the client side service. This includes the number of requests to the client service and whether the requests were faults or okay.
iClientRequestImpactStatistics :: Lens' Insight (Maybe RequestImpactStatistics)
iClientRequestImpactStatistics = lens _iClientRequestImpactStatistics (\s a -> s {_iClientRequestImpactStatistics = a})

-- | The time, in Unix seconds, at which the insight ended.
iEndTime :: Lens' Insight (Maybe UTCTime)
iEndTime = lens _iEndTime (\s a -> s {_iEndTime = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the group that the insight belongs to.
iGroupARN :: Lens' Insight (Maybe Text)
iGroupARN = lens _iGroupARN (\s a -> s {_iGroupARN = a})

-- | The name of the group that the insight belongs to.
iGroupName :: Lens' Insight (Maybe Text)
iGroupName = lens _iGroupName (\s a -> s {_iGroupName = a})

instance FromJSON Insight where
  parseJSON =
    withObject
      "Insight"
      ( \x ->
          Insight'
            <$> (x .:? "Summary")
            <*> (x .:? "State")
            <*> (x .:? "StartTime")
            <*> (x .:? "InsightId")
            <*> (x .:? "Categories" .!= mempty)
            <*> (x .:? "RootCauseServiceRequestImpactStatistics")
            <*> (x .:? "TopAnomalousServices" .!= mempty)
            <*> (x .:? "RootCauseServiceId")
            <*> (x .:? "ClientRequestImpactStatistics")
            <*> (x .:? "EndTime")
            <*> (x .:? "GroupARN")
            <*> (x .:? "GroupName")
      )

instance Hashable Insight

instance NFData Insight
