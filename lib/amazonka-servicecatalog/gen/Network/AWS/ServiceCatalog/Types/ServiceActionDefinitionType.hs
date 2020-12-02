{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ServiceActionDefinitionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ServiceActionDefinitionType where

import Network.AWS.Prelude

data ServiceActionDefinitionType = SsmAutomation
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

instance FromText ServiceActionDefinitionType where
  parser =
    takeLowerText >>= \case
      "ssm_automation" -> pure SsmAutomation
      e ->
        fromTextError $
          "Failure parsing ServiceActionDefinitionType from value: '" <> e
            <> "'. Accepted values: ssm_automation"

instance ToText ServiceActionDefinitionType where
  toText = \case
    SsmAutomation -> "SSM_AUTOMATION"

instance Hashable ServiceActionDefinitionType

instance NFData ServiceActionDefinitionType

instance ToByteString ServiceActionDefinitionType

instance ToQuery ServiceActionDefinitionType

instance ToHeader ServiceActionDefinitionType

instance ToJSON ServiceActionDefinitionType where
  toJSON = toJSONText

instance FromJSON ServiceActionDefinitionType where
  parseJSON = parseJSONText "ServiceActionDefinitionType"
