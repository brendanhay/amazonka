{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.ChangeType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.ChangeType where

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
      "requires-reboot" -> pure RequiresReboot
      e ->
        fromTextError $
          "Failure parsing ChangeType from value: '" <> e
            <> "'. Accepted values: immediate, requires-reboot"

instance ToText ChangeType where
  toText = \case
    Immediate -> "immediate"
    RequiresReboot -> "requires-reboot"

instance Hashable ChangeType

instance NFData ChangeType

instance ToByteString ChangeType

instance ToQuery ChangeType

instance ToHeader ChangeType

instance FromXML ChangeType where
  parseXML = parseXMLText "ChangeType"
