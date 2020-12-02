{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.EventRiskType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.EventRiskType where

import Network.AWS.CognitoIdentityProvider.Types.RiskDecisionType
import Network.AWS.CognitoIdentityProvider.Types.RiskLevelType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The event risk type.
--
--
--
-- /See:/ 'eventRiskType' smart constructor.
data EventRiskType = EventRiskType'
  { _ertCompromisedCredentialsDetected ::
      !(Maybe Bool),
    _ertRiskLevel :: !(Maybe RiskLevelType),
    _ertRiskDecision :: !(Maybe RiskDecisionType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EventRiskType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ertCompromisedCredentialsDetected' - Indicates whether compromised credentials were detected during an authentication event.
--
-- * 'ertRiskLevel' - The risk level.
--
-- * 'ertRiskDecision' - The risk decision.
eventRiskType ::
  EventRiskType
eventRiskType =
  EventRiskType'
    { _ertCompromisedCredentialsDetected = Nothing,
      _ertRiskLevel = Nothing,
      _ertRiskDecision = Nothing
    }

-- | Indicates whether compromised credentials were detected during an authentication event.
ertCompromisedCredentialsDetected :: Lens' EventRiskType (Maybe Bool)
ertCompromisedCredentialsDetected = lens _ertCompromisedCredentialsDetected (\s a -> s {_ertCompromisedCredentialsDetected = a})

-- | The risk level.
ertRiskLevel :: Lens' EventRiskType (Maybe RiskLevelType)
ertRiskLevel = lens _ertRiskLevel (\s a -> s {_ertRiskLevel = a})

-- | The risk decision.
ertRiskDecision :: Lens' EventRiskType (Maybe RiskDecisionType)
ertRiskDecision = lens _ertRiskDecision (\s a -> s {_ertRiskDecision = a})

instance FromJSON EventRiskType where
  parseJSON =
    withObject
      "EventRiskType"
      ( \x ->
          EventRiskType'
            <$> (x .:? "CompromisedCredentialsDetected")
            <*> (x .:? "RiskLevel")
            <*> (x .:? "RiskDecision")
      )

instance Hashable EventRiskType

instance NFData EventRiskType
