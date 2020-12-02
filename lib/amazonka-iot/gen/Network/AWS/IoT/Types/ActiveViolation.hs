{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ActiveViolation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ActiveViolation where

import Network.AWS.IoT.Types.Behavior
import Network.AWS.IoT.Types.MetricValue
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about an active Device Defender security profile behavior violation.
--
--
--
-- /See:/ 'activeViolation' smart constructor.
data ActiveViolation = ActiveViolation'
  { _avLastViolationValue ::
      !(Maybe MetricValue),
    _avLastViolationTime :: !(Maybe POSIX),
    _avViolationStartTime :: !(Maybe POSIX),
    _avViolationId :: !(Maybe Text),
    _avBehavior :: !(Maybe Behavior),
    _avSecurityProfileName :: !(Maybe Text),
    _avThingName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ActiveViolation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avLastViolationValue' - The value of the metric (the measurement) which caused the most recent violation.
--
-- * 'avLastViolationTime' - The time the most recent violation occurred.
--
-- * 'avViolationStartTime' - The time the violation started.
--
-- * 'avViolationId' - The ID of the active violation.
--
-- * 'avBehavior' - The behavior which is being violated.
--
-- * 'avSecurityProfileName' - The security profile whose behavior is in violation.
--
-- * 'avThingName' - The name of the thing responsible for the active violation.
activeViolation ::
  ActiveViolation
activeViolation =
  ActiveViolation'
    { _avLastViolationValue = Nothing,
      _avLastViolationTime = Nothing,
      _avViolationStartTime = Nothing,
      _avViolationId = Nothing,
      _avBehavior = Nothing,
      _avSecurityProfileName = Nothing,
      _avThingName = Nothing
    }

-- | The value of the metric (the measurement) which caused the most recent violation.
avLastViolationValue :: Lens' ActiveViolation (Maybe MetricValue)
avLastViolationValue = lens _avLastViolationValue (\s a -> s {_avLastViolationValue = a})

-- | The time the most recent violation occurred.
avLastViolationTime :: Lens' ActiveViolation (Maybe UTCTime)
avLastViolationTime = lens _avLastViolationTime (\s a -> s {_avLastViolationTime = a}) . mapping _Time

-- | The time the violation started.
avViolationStartTime :: Lens' ActiveViolation (Maybe UTCTime)
avViolationStartTime = lens _avViolationStartTime (\s a -> s {_avViolationStartTime = a}) . mapping _Time

-- | The ID of the active violation.
avViolationId :: Lens' ActiveViolation (Maybe Text)
avViolationId = lens _avViolationId (\s a -> s {_avViolationId = a})

-- | The behavior which is being violated.
avBehavior :: Lens' ActiveViolation (Maybe Behavior)
avBehavior = lens _avBehavior (\s a -> s {_avBehavior = a})

-- | The security profile whose behavior is in violation.
avSecurityProfileName :: Lens' ActiveViolation (Maybe Text)
avSecurityProfileName = lens _avSecurityProfileName (\s a -> s {_avSecurityProfileName = a})

-- | The name of the thing responsible for the active violation.
avThingName :: Lens' ActiveViolation (Maybe Text)
avThingName = lens _avThingName (\s a -> s {_avThingName = a})

instance FromJSON ActiveViolation where
  parseJSON =
    withObject
      "ActiveViolation"
      ( \x ->
          ActiveViolation'
            <$> (x .:? "lastViolationValue")
            <*> (x .:? "lastViolationTime")
            <*> (x .:? "violationStartTime")
            <*> (x .:? "violationId")
            <*> (x .:? "behavior")
            <*> (x .:? "securityProfileName")
            <*> (x .:? "thingName")
      )

instance Hashable ActiveViolation

instance NFData ActiveViolation
