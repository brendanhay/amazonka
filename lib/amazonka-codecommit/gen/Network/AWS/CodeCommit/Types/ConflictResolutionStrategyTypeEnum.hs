{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.ConflictResolutionStrategyTypeEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.ConflictResolutionStrategyTypeEnum where

import Network.AWS.Prelude

data ConflictResolutionStrategyTypeEnum
  = AcceptDestination
  | AcceptSource
  | Automerge
  | None
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

instance FromText ConflictResolutionStrategyTypeEnum where
  parser =
    takeLowerText >>= \case
      "accept_destination" -> pure AcceptDestination
      "accept_source" -> pure AcceptSource
      "automerge" -> pure Automerge
      "none" -> pure None
      e ->
        fromTextError $
          "Failure parsing ConflictResolutionStrategyTypeEnum from value: '" <> e
            <> "'. Accepted values: accept_destination, accept_source, automerge, none"

instance ToText ConflictResolutionStrategyTypeEnum where
  toText = \case
    AcceptDestination -> "ACCEPT_DESTINATION"
    AcceptSource -> "ACCEPT_SOURCE"
    Automerge -> "AUTOMERGE"
    None -> "NONE"

instance Hashable ConflictResolutionStrategyTypeEnum

instance NFData ConflictResolutionStrategyTypeEnum

instance ToByteString ConflictResolutionStrategyTypeEnum

instance ToQuery ConflictResolutionStrategyTypeEnum

instance ToHeader ConflictResolutionStrategyTypeEnum

instance ToJSON ConflictResolutionStrategyTypeEnum where
  toJSON = toJSONText
