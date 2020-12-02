{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.TelemetryRecord
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.TelemetryRecord where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.XRay.Types.BackendConnectionErrors

-- |
--
--
--
-- /See:/ 'telemetryRecord' smart constructor.
data TelemetryRecord = TelemetryRecord'
  { _trSegmentsReceivedCount ::
      !(Maybe Int),
    _trSegmentsSentCount :: !(Maybe Int),
    _trSegmentsSpilloverCount :: !(Maybe Int),
    _trSegmentsRejectedCount :: !(Maybe Int),
    _trBackendConnectionErrors ::
      !(Maybe BackendConnectionErrors),
    _trTimestamp :: !POSIX
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TelemetryRecord' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trSegmentsReceivedCount' -
--
-- * 'trSegmentsSentCount' -
--
-- * 'trSegmentsSpilloverCount' -
--
-- * 'trSegmentsRejectedCount' -
--
-- * 'trBackendConnectionErrors' -
--
-- * 'trTimestamp' -
telemetryRecord ::
  -- | 'trTimestamp'
  UTCTime ->
  TelemetryRecord
telemetryRecord pTimestamp_ =
  TelemetryRecord'
    { _trSegmentsReceivedCount = Nothing,
      _trSegmentsSentCount = Nothing,
      _trSegmentsSpilloverCount = Nothing,
      _trSegmentsRejectedCount = Nothing,
      _trBackendConnectionErrors = Nothing,
      _trTimestamp = _Time # pTimestamp_
    }

-- |
trSegmentsReceivedCount :: Lens' TelemetryRecord (Maybe Int)
trSegmentsReceivedCount = lens _trSegmentsReceivedCount (\s a -> s {_trSegmentsReceivedCount = a})

-- |
trSegmentsSentCount :: Lens' TelemetryRecord (Maybe Int)
trSegmentsSentCount = lens _trSegmentsSentCount (\s a -> s {_trSegmentsSentCount = a})

-- |
trSegmentsSpilloverCount :: Lens' TelemetryRecord (Maybe Int)
trSegmentsSpilloverCount = lens _trSegmentsSpilloverCount (\s a -> s {_trSegmentsSpilloverCount = a})

-- |
trSegmentsRejectedCount :: Lens' TelemetryRecord (Maybe Int)
trSegmentsRejectedCount = lens _trSegmentsRejectedCount (\s a -> s {_trSegmentsRejectedCount = a})

-- |
trBackendConnectionErrors :: Lens' TelemetryRecord (Maybe BackendConnectionErrors)
trBackendConnectionErrors = lens _trBackendConnectionErrors (\s a -> s {_trBackendConnectionErrors = a})

-- |
trTimestamp :: Lens' TelemetryRecord UTCTime
trTimestamp = lens _trTimestamp (\s a -> s {_trTimestamp = a}) . _Time

instance Hashable TelemetryRecord

instance NFData TelemetryRecord

instance ToJSON TelemetryRecord where
  toJSON TelemetryRecord' {..} =
    object
      ( catMaybes
          [ ("SegmentsReceivedCount" .=) <$> _trSegmentsReceivedCount,
            ("SegmentsSentCount" .=) <$> _trSegmentsSentCount,
            ("SegmentsSpilloverCount" .=) <$> _trSegmentsSpilloverCount,
            ("SegmentsRejectedCount" .=) <$> _trSegmentsRejectedCount,
            ("BackendConnectionErrors" .=) <$> _trBackendConnectionErrors,
            Just ("Timestamp" .= _trTimestamp)
          ]
      )
