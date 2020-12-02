{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.LogsConfigStatusType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.LogsConfigStatusType where

import Network.AWS.Prelude

data LogsConfigStatusType
  = Disabled
  | Enabled
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

instance FromText LogsConfigStatusType where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure Disabled
      "enabled" -> pure Enabled
      e ->
        fromTextError $
          "Failure parsing LogsConfigStatusType from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText LogsConfigStatusType where
  toText = \case
    Disabled -> "DISABLED"
    Enabled -> "ENABLED"

instance Hashable LogsConfigStatusType

instance NFData LogsConfigStatusType

instance ToByteString LogsConfigStatusType

instance ToQuery LogsConfigStatusType

instance ToHeader LogsConfigStatusType

instance ToJSON LogsConfigStatusType where
  toJSON = toJSONText

instance FromJSON LogsConfigStatusType where
  parseJSON = parseJSONText "LogsConfigStatusType"
