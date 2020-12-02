{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TargetPlatformOS
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TargetPlatformOS where

import Network.AWS.Prelude

data TargetPlatformOS
  = Android
  | Linux
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

instance FromText TargetPlatformOS where
  parser =
    takeLowerText >>= \case
      "android" -> pure Android
      "linux" -> pure Linux
      e ->
        fromTextError $
          "Failure parsing TargetPlatformOS from value: '" <> e
            <> "'. Accepted values: android, linux"

instance ToText TargetPlatformOS where
  toText = \case
    Android -> "ANDROID"
    Linux -> "LINUX"

instance Hashable TargetPlatformOS

instance NFData TargetPlatformOS

instance ToByteString TargetPlatformOS

instance ToQuery TargetPlatformOS

instance ToHeader TargetPlatformOS

instance ToJSON TargetPlatformOS where
  toJSON = toJSONText

instance FromJSON TargetPlatformOS where
  parseJSON = parseJSONText "TargetPlatformOS"
