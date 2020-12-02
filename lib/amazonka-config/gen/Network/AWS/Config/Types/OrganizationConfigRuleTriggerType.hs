{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.OrganizationConfigRuleTriggerType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.OrganizationConfigRuleTriggerType where

import Network.AWS.Prelude

data OrganizationConfigRuleTriggerType
  = OCRTTConfigurationItemChangeNotification
  | OCRTTOversizedConfigurationItemChangeNotification
  | OCRTTScheduledNotification
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText OrganizationConfigRuleTriggerType where
  parser =
    takeLowerText >>= \case
      "configurationitemchangenotification" -> pure OCRTTConfigurationItemChangeNotification
      "oversizedconfigurationitemchangenotification" -> pure OCRTTOversizedConfigurationItemChangeNotification
      "schedulednotification" -> pure OCRTTScheduledNotification
      e ->
        fromTextError $
          "Failure parsing OrganizationConfigRuleTriggerType from value: '" <> e
            <> "'. Accepted values: configurationitemchangenotification, oversizedconfigurationitemchangenotification, schedulednotification"

instance ToText OrganizationConfigRuleTriggerType where
  toText = \case
    OCRTTConfigurationItemChangeNotification -> "ConfigurationItemChangeNotification"
    OCRTTOversizedConfigurationItemChangeNotification -> "OversizedConfigurationItemChangeNotification"
    OCRTTScheduledNotification -> "ScheduledNotification"

instance Hashable OrganizationConfigRuleTriggerType

instance NFData OrganizationConfigRuleTriggerType

instance ToByteString OrganizationConfigRuleTriggerType

instance ToQuery OrganizationConfigRuleTriggerType

instance ToHeader OrganizationConfigRuleTriggerType

instance ToJSON OrganizationConfigRuleTriggerType where
  toJSON = toJSONText

instance FromJSON OrganizationConfigRuleTriggerType where
  parseJSON = parseJSONText "OrganizationConfigRuleTriggerType"
