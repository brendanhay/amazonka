{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.SamplingStatisticsDocument
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.SamplingStatisticsDocument where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Request sampling results for a single rule from a service. Results are for the last 10 seconds unless the service has been assigned a longer reporting interval after a previous call to 'GetSamplingTargets' .
--
--
--
-- /See:/ 'samplingStatisticsDocument' smart constructor.
data SamplingStatisticsDocument = SamplingStatisticsDocument'
  { _ssdBorrowCount ::
      !(Maybe Nat),
    _ssdRuleName :: !Text,
    _ssdClientId :: !Text,
    _ssdTimestamp :: !POSIX,
    _ssdRequestCount :: !Nat,
    _ssdSampledCount :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SamplingStatisticsDocument' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssdBorrowCount' - The number of requests recorded with borrowed reservoir quota.
--
-- * 'ssdRuleName' - The name of the sampling rule.
--
-- * 'ssdClientId' - A unique identifier for the service in hexadecimal.
--
-- * 'ssdTimestamp' - The current time.
--
-- * 'ssdRequestCount' - The number of requests that matched the rule.
--
-- * 'ssdSampledCount' - The number of requests recorded.
samplingStatisticsDocument ::
  -- | 'ssdRuleName'
  Text ->
  -- | 'ssdClientId'
  Text ->
  -- | 'ssdTimestamp'
  UTCTime ->
  -- | 'ssdRequestCount'
  Natural ->
  -- | 'ssdSampledCount'
  Natural ->
  SamplingStatisticsDocument
samplingStatisticsDocument
  pRuleName_
  pClientId_
  pTimestamp_
  pRequestCount_
  pSampledCount_ =
    SamplingStatisticsDocument'
      { _ssdBorrowCount = Nothing,
        _ssdRuleName = pRuleName_,
        _ssdClientId = pClientId_,
        _ssdTimestamp = _Time # pTimestamp_,
        _ssdRequestCount = _Nat # pRequestCount_,
        _ssdSampledCount = _Nat # pSampledCount_
      }

-- | The number of requests recorded with borrowed reservoir quota.
ssdBorrowCount :: Lens' SamplingStatisticsDocument (Maybe Natural)
ssdBorrowCount = lens _ssdBorrowCount (\s a -> s {_ssdBorrowCount = a}) . mapping _Nat

-- | The name of the sampling rule.
ssdRuleName :: Lens' SamplingStatisticsDocument Text
ssdRuleName = lens _ssdRuleName (\s a -> s {_ssdRuleName = a})

-- | A unique identifier for the service in hexadecimal.
ssdClientId :: Lens' SamplingStatisticsDocument Text
ssdClientId = lens _ssdClientId (\s a -> s {_ssdClientId = a})

-- | The current time.
ssdTimestamp :: Lens' SamplingStatisticsDocument UTCTime
ssdTimestamp = lens _ssdTimestamp (\s a -> s {_ssdTimestamp = a}) . _Time

-- | The number of requests that matched the rule.
ssdRequestCount :: Lens' SamplingStatisticsDocument Natural
ssdRequestCount = lens _ssdRequestCount (\s a -> s {_ssdRequestCount = a}) . _Nat

-- | The number of requests recorded.
ssdSampledCount :: Lens' SamplingStatisticsDocument Natural
ssdSampledCount = lens _ssdSampledCount (\s a -> s {_ssdSampledCount = a}) . _Nat

instance Hashable SamplingStatisticsDocument

instance NFData SamplingStatisticsDocument

instance ToJSON SamplingStatisticsDocument where
  toJSON SamplingStatisticsDocument' {..} =
    object
      ( catMaybes
          [ ("BorrowCount" .=) <$> _ssdBorrowCount,
            Just ("RuleName" .= _ssdRuleName),
            Just ("ClientID" .= _ssdClientId),
            Just ("Timestamp" .= _ssdTimestamp),
            Just ("RequestCount" .= _ssdRequestCount),
            Just ("SampledCount" .= _ssdSampledCount)
          ]
      )
