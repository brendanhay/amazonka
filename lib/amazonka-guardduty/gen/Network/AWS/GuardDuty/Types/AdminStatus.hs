{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.AdminStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.AdminStatus where

import Network.AWS.Prelude

data AdminStatus
  = DisableInProgress
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

instance FromText AdminStatus where
  parser =
    takeLowerText >>= \case
      "disable_in_progress" -> pure DisableInProgress
      "enabled" -> pure Enabled
      e ->
        fromTextError $
          "Failure parsing AdminStatus from value: '" <> e
            <> "'. Accepted values: disable_in_progress, enabled"

instance ToText AdminStatus where
  toText = \case
    DisableInProgress -> "DISABLE_IN_PROGRESS"
    Enabled -> "ENABLED"

instance Hashable AdminStatus

instance NFData AdminStatus

instance ToByteString AdminStatus

instance ToQuery AdminStatus

instance ToHeader AdminStatus

instance FromJSON AdminStatus where
  parseJSON = parseJSONText "AdminStatus"
