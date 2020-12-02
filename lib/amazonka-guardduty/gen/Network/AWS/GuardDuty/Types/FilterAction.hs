{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.FilterAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.FilterAction where

import Network.AWS.Prelude

data FilterAction
  = Archive
  | Noop
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

instance FromText FilterAction where
  parser =
    takeLowerText >>= \case
      "archive" -> pure Archive
      "noop" -> pure Noop
      e ->
        fromTextError $
          "Failure parsing FilterAction from value: '" <> e
            <> "'. Accepted values: archive, noop"

instance ToText FilterAction where
  toText = \case
    Archive -> "ARCHIVE"
    Noop -> "NOOP"

instance Hashable FilterAction

instance NFData FilterAction

instance ToByteString FilterAction

instance ToQuery FilterAction

instance ToHeader FilterAction

instance ToJSON FilterAction where
  toJSON = toJSONText

instance FromJSON FilterAction where
  parseJSON = parseJSONText "FilterAction"
