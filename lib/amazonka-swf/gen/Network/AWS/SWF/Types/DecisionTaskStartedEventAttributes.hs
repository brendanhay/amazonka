{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.DecisionTaskStartedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.DecisionTaskStartedEventAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides the details of the @DecisionTaskStarted@ event.
--
--
--
-- /See:/ 'decisionTaskStartedEventAttributes' smart constructor.
data DecisionTaskStartedEventAttributes = DecisionTaskStartedEventAttributes'
  { _dtseaIdentity ::
      !(Maybe Text),
    _dtseaScheduledEventId ::
      !Integer
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DecisionTaskStartedEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtseaIdentity' - Identity of the decider making the request. This enables diagnostic tracing when problems arise. The form of this identity is user defined.
--
-- * 'dtseaScheduledEventId' - The ID of the @DecisionTaskScheduled@ event that was recorded when this decision task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
decisionTaskStartedEventAttributes ::
  -- | 'dtseaScheduledEventId'
  Integer ->
  DecisionTaskStartedEventAttributes
decisionTaskStartedEventAttributes pScheduledEventId_ =
  DecisionTaskStartedEventAttributes'
    { _dtseaIdentity = Nothing,
      _dtseaScheduledEventId = pScheduledEventId_
    }

-- | Identity of the decider making the request. This enables diagnostic tracing when problems arise. The form of this identity is user defined.
dtseaIdentity :: Lens' DecisionTaskStartedEventAttributes (Maybe Text)
dtseaIdentity = lens _dtseaIdentity (\s a -> s {_dtseaIdentity = a})

-- | The ID of the @DecisionTaskScheduled@ event that was recorded when this decision task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
dtseaScheduledEventId :: Lens' DecisionTaskStartedEventAttributes Integer
dtseaScheduledEventId = lens _dtseaScheduledEventId (\s a -> s {_dtseaScheduledEventId = a})

instance FromJSON DecisionTaskStartedEventAttributes where
  parseJSON =
    withObject
      "DecisionTaskStartedEventAttributes"
      ( \x ->
          DecisionTaskStartedEventAttributes'
            <$> (x .:? "identity") <*> (x .: "scheduledEventId")
      )

instance Hashable DecisionTaskStartedEventAttributes

instance NFData DecisionTaskStartedEventAttributes
