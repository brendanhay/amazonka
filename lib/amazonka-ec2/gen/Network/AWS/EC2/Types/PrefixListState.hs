{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PrefixListState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PrefixListState where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data PrefixListState
  = CreateComplete
  | CreateFailed
  | CreateInProgress
  | DeleteComplete
  | DeleteFailed
  | DeleteInProgress
  | ModifyComplete
  | ModifyFailed
  | ModifyInProgress
  | RestoreComplete
  | RestoreFailed
  | RestoreInProgress
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

instance FromText PrefixListState where
  parser =
    takeLowerText >>= \case
      "create-complete" -> pure CreateComplete
      "create-failed" -> pure CreateFailed
      "create-in-progress" -> pure CreateInProgress
      "delete-complete" -> pure DeleteComplete
      "delete-failed" -> pure DeleteFailed
      "delete-in-progress" -> pure DeleteInProgress
      "modify-complete" -> pure ModifyComplete
      "modify-failed" -> pure ModifyFailed
      "modify-in-progress" -> pure ModifyInProgress
      "restore-complete" -> pure RestoreComplete
      "restore-failed" -> pure RestoreFailed
      "restore-in-progress" -> pure RestoreInProgress
      e ->
        fromTextError $
          "Failure parsing PrefixListState from value: '" <> e
            <> "'. Accepted values: create-complete, create-failed, create-in-progress, delete-complete, delete-failed, delete-in-progress, modify-complete, modify-failed, modify-in-progress, restore-complete, restore-failed, restore-in-progress"

instance ToText PrefixListState where
  toText = \case
    CreateComplete -> "create-complete"
    CreateFailed -> "create-failed"
    CreateInProgress -> "create-in-progress"
    DeleteComplete -> "delete-complete"
    DeleteFailed -> "delete-failed"
    DeleteInProgress -> "delete-in-progress"
    ModifyComplete -> "modify-complete"
    ModifyFailed -> "modify-failed"
    ModifyInProgress -> "modify-in-progress"
    RestoreComplete -> "restore-complete"
    RestoreFailed -> "restore-failed"
    RestoreInProgress -> "restore-in-progress"

instance Hashable PrefixListState

instance NFData PrefixListState

instance ToByteString PrefixListState

instance ToQuery PrefixListState

instance ToHeader PrefixListState

instance FromXML PrefixListState where
  parseXML = parseXMLText "PrefixListState"
