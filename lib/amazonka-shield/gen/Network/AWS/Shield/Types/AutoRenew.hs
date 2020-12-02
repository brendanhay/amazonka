{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.AutoRenew
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.AutoRenew where

import Network.AWS.Prelude

data AutoRenew
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

instance FromText AutoRenew where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure Disabled
      "enabled" -> pure Enabled
      e ->
        fromTextError $
          "Failure parsing AutoRenew from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText AutoRenew where
  toText = \case
    Disabled -> "DISABLED"
    Enabled -> "ENABLED"

instance Hashable AutoRenew

instance NFData AutoRenew

instance ToByteString AutoRenew

instance ToQuery AutoRenew

instance ToHeader AutoRenew

instance ToJSON AutoRenew where
  toJSON = toJSONText

instance FromJSON AutoRenew where
  parseJSON = parseJSONText "AutoRenew"
