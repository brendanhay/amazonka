{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.DocumentType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.DocumentType where

import Network.AWS.Prelude

data DocumentType
  = DTApplicationConfiguration
  | DTApplicationConfigurationSchema
  | DTAutomation
  | DTChangeCalendar
  | DTCommand
  | DTDeploymentStrategy
  | DTPackage
  | DTPolicy
  | DTSession
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

instance FromText DocumentType where
  parser =
    takeLowerText >>= \case
      "applicationconfiguration" -> pure DTApplicationConfiguration
      "applicationconfigurationschema" -> pure DTApplicationConfigurationSchema
      "automation" -> pure DTAutomation
      "changecalendar" -> pure DTChangeCalendar
      "command" -> pure DTCommand
      "deploymentstrategy" -> pure DTDeploymentStrategy
      "package" -> pure DTPackage
      "policy" -> pure DTPolicy
      "session" -> pure DTSession
      e ->
        fromTextError $
          "Failure parsing DocumentType from value: '" <> e
            <> "'. Accepted values: applicationconfiguration, applicationconfigurationschema, automation, changecalendar, command, deploymentstrategy, package, policy, session"

instance ToText DocumentType where
  toText = \case
    DTApplicationConfiguration -> "ApplicationConfiguration"
    DTApplicationConfigurationSchema -> "ApplicationConfigurationSchema"
    DTAutomation -> "Automation"
    DTChangeCalendar -> "ChangeCalendar"
    DTCommand -> "Command"
    DTDeploymentStrategy -> "DeploymentStrategy"
    DTPackage -> "Package"
    DTPolicy -> "Policy"
    DTSession -> "Session"

instance Hashable DocumentType

instance NFData DocumentType

instance ToByteString DocumentType

instance ToQuery DocumentType

instance ToHeader DocumentType

instance ToJSON DocumentType where
  toJSON = toJSONText

instance FromJSON DocumentType where
  parseJSON = parseJSONText "DocumentType"
