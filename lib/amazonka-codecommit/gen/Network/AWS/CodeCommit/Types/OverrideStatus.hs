{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.OverrideStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.OverrideStatus where

import Network.AWS.Prelude

data OverrideStatus
  = Override
  | Revoke
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

instance FromText OverrideStatus where
  parser =
    takeLowerText >>= \case
      "override" -> pure Override
      "revoke" -> pure Revoke
      e ->
        fromTextError $
          "Failure parsing OverrideStatus from value: '" <> e
            <> "'. Accepted values: override, revoke"

instance ToText OverrideStatus where
  toText = \case
    Override -> "OVERRIDE"
    Revoke -> "REVOKE"

instance Hashable OverrideStatus

instance NFData OverrideStatus

instance ToByteString OverrideStatus

instance ToQuery OverrideStatus

instance ToHeader OverrideStatus

instance ToJSON OverrideStatus where
  toJSON = toJSONText

instance FromJSON OverrideStatus where
  parseJSON = parseJSONText "OverrideStatus"
