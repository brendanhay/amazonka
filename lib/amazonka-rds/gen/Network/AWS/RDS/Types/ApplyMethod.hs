{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.ApplyMethod
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.ApplyMethod where

import Network.AWS.Prelude

data ApplyMethod
  = Immediate
  | PendingReboot
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

instance FromText ApplyMethod where
  parser =
    takeLowerText >>= \case
      "immediate" -> pure Immediate
      "pending-reboot" -> pure PendingReboot
      e ->
        fromTextError $
          "Failure parsing ApplyMethod from value: '" <> e
            <> "'. Accepted values: immediate, pending-reboot"

instance ToText ApplyMethod where
  toText = \case
    Immediate -> "immediate"
    PendingReboot -> "pending-reboot"

instance Hashable ApplyMethod

instance NFData ApplyMethod

instance ToByteString ApplyMethod

instance ToQuery ApplyMethod

instance ToHeader ApplyMethod

instance FromXML ApplyMethod where
  parseXML = parseXMLText "ApplyMethod"
