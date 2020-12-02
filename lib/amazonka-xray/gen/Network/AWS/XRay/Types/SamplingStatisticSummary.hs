{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.SamplingStatisticSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.SamplingStatisticSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Aggregated request sampling data for a sampling rule across all services for a 10-second window.
--
--
--
-- /See:/ 'samplingStatisticSummary' smart constructor.
data SamplingStatisticSummary = SamplingStatisticSummary'
  { _sssRequestCount ::
      !(Maybe Int),
    _sssBorrowCount :: !(Maybe Int),
    _sssRuleName :: !(Maybe Text),
    _sssTimestamp :: !(Maybe POSIX),
    _sssSampledCount :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SamplingStatisticSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sssRequestCount' - The number of requests that matched the rule.
--
-- * 'sssBorrowCount' - The number of requests recorded with borrowed reservoir quota.
--
-- * 'sssRuleName' - The name of the sampling rule.
--
-- * 'sssTimestamp' - The start time of the reporting window.
--
-- * 'sssSampledCount' - The number of requests recorded.
samplingStatisticSummary ::
  SamplingStatisticSummary
samplingStatisticSummary =
  SamplingStatisticSummary'
    { _sssRequestCount = Nothing,
      _sssBorrowCount = Nothing,
      _sssRuleName = Nothing,
      _sssTimestamp = Nothing,
      _sssSampledCount = Nothing
    }

-- | The number of requests that matched the rule.
sssRequestCount :: Lens' SamplingStatisticSummary (Maybe Int)
sssRequestCount = lens _sssRequestCount (\s a -> s {_sssRequestCount = a})

-- | The number of requests recorded with borrowed reservoir quota.
sssBorrowCount :: Lens' SamplingStatisticSummary (Maybe Int)
sssBorrowCount = lens _sssBorrowCount (\s a -> s {_sssBorrowCount = a})

-- | The name of the sampling rule.
sssRuleName :: Lens' SamplingStatisticSummary (Maybe Text)
sssRuleName = lens _sssRuleName (\s a -> s {_sssRuleName = a})

-- | The start time of the reporting window.
sssTimestamp :: Lens' SamplingStatisticSummary (Maybe UTCTime)
sssTimestamp = lens _sssTimestamp (\s a -> s {_sssTimestamp = a}) . mapping _Time

-- | The number of requests recorded.
sssSampledCount :: Lens' SamplingStatisticSummary (Maybe Int)
sssSampledCount = lens _sssSampledCount (\s a -> s {_sssSampledCount = a})

instance FromJSON SamplingStatisticSummary where
  parseJSON =
    withObject
      "SamplingStatisticSummary"
      ( \x ->
          SamplingStatisticSummary'
            <$> (x .:? "RequestCount")
            <*> (x .:? "BorrowCount")
            <*> (x .:? "RuleName")
            <*> (x .:? "Timestamp")
            <*> (x .:? "SampledCount")
      )

instance Hashable SamplingStatisticSummary

instance NFData SamplingStatisticSummary
