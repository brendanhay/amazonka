{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.Types.ChangeType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DAX.Types.ChangeType where

import Network.AWS.Prelude

data ChangeType
  = Immediate
  | RequiresReboot
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

instance FromText ChangeType where
  parser =
    takeLowerText >>= \case
      "immediate" -> pure Immediate
      "requires_reboot" -> pure RequiresReboot
      e ->
        fromTextError $
          "Failure parsing ChangeType from value: '" <> e
            <> "'. Accepted values: immediate, requires_reboot"

instance ToText ChangeType where
  toText = \case
    Immediate -> "IMMEDIATE"
    RequiresReboot -> "REQUIRES_REBOOT"

instance Hashable ChangeType

instance NFData ChangeType

instance ToByteString ChangeType

instance ToQuery ChangeType

instance ToHeader ChangeType

instance FromJSON ChangeType where
  parseJSON = parseJSONText "ChangeType"
