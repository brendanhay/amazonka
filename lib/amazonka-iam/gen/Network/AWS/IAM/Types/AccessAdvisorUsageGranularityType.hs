{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.AccessAdvisorUsageGranularityType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.AccessAdvisorUsageGranularityType where

import Network.AWS.Prelude

data AccessAdvisorUsageGranularityType
  = ActionLevel
  | ServiceLevel
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

instance FromText AccessAdvisorUsageGranularityType where
  parser =
    takeLowerText >>= \case
      "action_level" -> pure ActionLevel
      "service_level" -> pure ServiceLevel
      e ->
        fromTextError $
          "Failure parsing AccessAdvisorUsageGranularityType from value: '" <> e
            <> "'. Accepted values: action_level, service_level"

instance ToText AccessAdvisorUsageGranularityType where
  toText = \case
    ActionLevel -> "ACTION_LEVEL"
    ServiceLevel -> "SERVICE_LEVEL"

instance Hashable AccessAdvisorUsageGranularityType

instance NFData AccessAdvisorUsageGranularityType

instance ToByteString AccessAdvisorUsageGranularityType

instance ToQuery AccessAdvisorUsageGranularityType

instance ToHeader AccessAdvisorUsageGranularityType

instance FromXML AccessAdvisorUsageGranularityType where
  parseXML = parseXMLText "AccessAdvisorUsageGranularityType"
