{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.ModificationStateEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.ModificationStateEnum where

import Network.AWS.Prelude

data ModificationStateEnum
  = UpdateInProgress
  | UpdateInitiated
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

instance FromText ModificationStateEnum where
  parser =
    takeLowerText >>= \case
      "update_in_progress" -> pure UpdateInProgress
      "update_initiated" -> pure UpdateInitiated
      e ->
        fromTextError $
          "Failure parsing ModificationStateEnum from value: '" <> e
            <> "'. Accepted values: update_in_progress, update_initiated"

instance ToText ModificationStateEnum where
  toText = \case
    UpdateInProgress -> "UPDATE_IN_PROGRESS"
    UpdateInitiated -> "UPDATE_INITIATED"

instance Hashable ModificationStateEnum

instance NFData ModificationStateEnum

instance ToByteString ModificationStateEnum

instance ToQuery ModificationStateEnum

instance ToHeader ModificationStateEnum

instance FromJSON ModificationStateEnum where
  parseJSON = parseJSONText "ModificationStateEnum"
