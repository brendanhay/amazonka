{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.AppLaunchConfigurationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.AppLaunchConfigurationStatus where

import Network.AWS.Prelude

data AppLaunchConfigurationStatus
  = ALCSConfigured
  | ALCSNotConfigured
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

instance FromText AppLaunchConfigurationStatus where
  parser =
    takeLowerText >>= \case
      "configured" -> pure ALCSConfigured
      "not_configured" -> pure ALCSNotConfigured
      e ->
        fromTextError $
          "Failure parsing AppLaunchConfigurationStatus from value: '" <> e
            <> "'. Accepted values: configured, not_configured"

instance ToText AppLaunchConfigurationStatus where
  toText = \case
    ALCSConfigured -> "CONFIGURED"
    ALCSNotConfigured -> "NOT_CONFIGURED"

instance Hashable AppLaunchConfigurationStatus

instance NFData AppLaunchConfigurationStatus

instance ToByteString AppLaunchConfigurationStatus

instance ToQuery AppLaunchConfigurationStatus

instance ToHeader AppLaunchConfigurationStatus

instance FromJSON AppLaunchConfigurationStatus where
  parseJSON = parseJSONText "AppLaunchConfigurationStatus"
