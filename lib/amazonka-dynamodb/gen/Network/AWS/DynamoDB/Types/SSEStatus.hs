{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.SSEStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.SSEStatus where

import Network.AWS.Prelude

data SSEStatus
  = SSESDisabled
  | SSESDisabling
  | SSESEnabled
  | SSESEnabling
  | SSESUpdating
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

instance FromText SSEStatus where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure SSESDisabled
      "disabling" -> pure SSESDisabling
      "enabled" -> pure SSESEnabled
      "enabling" -> pure SSESEnabling
      "updating" -> pure SSESUpdating
      e ->
        fromTextError $
          "Failure parsing SSEStatus from value: '" <> e
            <> "'. Accepted values: disabled, disabling, enabled, enabling, updating"

instance ToText SSEStatus where
  toText = \case
    SSESDisabled -> "DISABLED"
    SSESDisabling -> "DISABLING"
    SSESEnabled -> "ENABLED"
    SSESEnabling -> "ENABLING"
    SSESUpdating -> "UPDATING"

instance Hashable SSEStatus

instance NFData SSEStatus

instance ToByteString SSEStatus

instance ToQuery SSEStatus

instance ToHeader SSEStatus

instance FromJSON SSEStatus where
  parseJSON = parseJSONText "SSEStatus"
