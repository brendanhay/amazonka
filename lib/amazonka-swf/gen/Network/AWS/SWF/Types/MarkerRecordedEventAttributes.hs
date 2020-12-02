{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.MarkerRecordedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.MarkerRecordedEventAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides the details of the @MarkerRecorded@ event.
--
--
--
-- /See:/ 'markerRecordedEventAttributes' smart constructor.
data MarkerRecordedEventAttributes = MarkerRecordedEventAttributes'
  { _mreaDetails ::
      !(Maybe Text),
    _mreaMarkerName :: !Text,
    _mreaDecisionTaskCompletedEventId ::
      !Integer
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MarkerRecordedEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mreaDetails' - The details of the marker.
--
-- * 'mreaMarkerName' - The name of the marker.
--
-- * 'mreaDecisionTaskCompletedEventId' - The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @RecordMarker@ decision that requested this marker. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
markerRecordedEventAttributes ::
  -- | 'mreaMarkerName'
  Text ->
  -- | 'mreaDecisionTaskCompletedEventId'
  Integer ->
  MarkerRecordedEventAttributes
markerRecordedEventAttributes
  pMarkerName_
  pDecisionTaskCompletedEventId_ =
    MarkerRecordedEventAttributes'
      { _mreaDetails = Nothing,
        _mreaMarkerName = pMarkerName_,
        _mreaDecisionTaskCompletedEventId =
          pDecisionTaskCompletedEventId_
      }

-- | The details of the marker.
mreaDetails :: Lens' MarkerRecordedEventAttributes (Maybe Text)
mreaDetails = lens _mreaDetails (\s a -> s {_mreaDetails = a})

-- | The name of the marker.
mreaMarkerName :: Lens' MarkerRecordedEventAttributes Text
mreaMarkerName = lens _mreaMarkerName (\s a -> s {_mreaMarkerName = a})

-- | The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @RecordMarker@ decision that requested this marker. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
mreaDecisionTaskCompletedEventId :: Lens' MarkerRecordedEventAttributes Integer
mreaDecisionTaskCompletedEventId = lens _mreaDecisionTaskCompletedEventId (\s a -> s {_mreaDecisionTaskCompletedEventId = a})

instance FromJSON MarkerRecordedEventAttributes where
  parseJSON =
    withObject
      "MarkerRecordedEventAttributes"
      ( \x ->
          MarkerRecordedEventAttributes'
            <$> (x .:? "details")
            <*> (x .: "markerName")
            <*> (x .: "decisionTaskCompletedEventId")
      )

instance Hashable MarkerRecordedEventAttributes

instance NFData MarkerRecordedEventAttributes
