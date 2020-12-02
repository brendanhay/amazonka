{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.DomainConfigurationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.DomainConfigurationStatus where

import Network.AWS.Prelude

data DomainConfigurationStatus
  = DCSDisabled
  | DCSEnabled
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

instance FromText DomainConfigurationStatus where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure DCSDisabled
      "enabled" -> pure DCSEnabled
      e ->
        fromTextError $
          "Failure parsing DomainConfigurationStatus from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText DomainConfigurationStatus where
  toText = \case
    DCSDisabled -> "DISABLED"
    DCSEnabled -> "ENABLED"

instance Hashable DomainConfigurationStatus

instance NFData DomainConfigurationStatus

instance ToByteString DomainConfigurationStatus

instance ToQuery DomainConfigurationStatus

instance ToHeader DomainConfigurationStatus

instance ToJSON DomainConfigurationStatus where
  toJSON = toJSONText

instance FromJSON DomainConfigurationStatus where
  parseJSON = parseJSONText "DomainConfigurationStatus"
