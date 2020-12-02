{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.NotificationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.NotificationType where

import Network.AWS.Prelude

data NotificationType
  = Command
  | Invocation
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

instance FromText NotificationType where
  parser =
    takeLowerText >>= \case
      "command" -> pure Command
      "invocation" -> pure Invocation
      e ->
        fromTextError $
          "Failure parsing NotificationType from value: '" <> e
            <> "'. Accepted values: command, invocation"

instance ToText NotificationType where
  toText = \case
    Command -> "Command"
    Invocation -> "Invocation"

instance Hashable NotificationType

instance NFData NotificationType

instance ToByteString NotificationType

instance ToQuery NotificationType

instance ToHeader NotificationType

instance ToJSON NotificationType where
  toJSON = toJSONText

instance FromJSON NotificationType where
  parseJSON = parseJSONText "NotificationType"
