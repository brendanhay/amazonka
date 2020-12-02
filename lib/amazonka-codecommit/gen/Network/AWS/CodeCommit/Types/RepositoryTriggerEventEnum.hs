{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.RepositoryTriggerEventEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.RepositoryTriggerEventEnum where

import Network.AWS.Prelude

data RepositoryTriggerEventEnum
  = All
  | CreateReference
  | DeleteReference
  | UpdateReference
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

instance FromText RepositoryTriggerEventEnum where
  parser =
    takeLowerText >>= \case
      "all" -> pure All
      "createreference" -> pure CreateReference
      "deletereference" -> pure DeleteReference
      "updatereference" -> pure UpdateReference
      e ->
        fromTextError $
          "Failure parsing RepositoryTriggerEventEnum from value: '" <> e
            <> "'. Accepted values: all, createreference, deletereference, updatereference"

instance ToText RepositoryTriggerEventEnum where
  toText = \case
    All -> "all"
    CreateReference -> "createReference"
    DeleteReference -> "deleteReference"
    UpdateReference -> "updateReference"

instance Hashable RepositoryTriggerEventEnum

instance NFData RepositoryTriggerEventEnum

instance ToByteString RepositoryTriggerEventEnum

instance ToQuery RepositoryTriggerEventEnum

instance ToHeader RepositoryTriggerEventEnum

instance ToJSON RepositoryTriggerEventEnum where
  toJSON = toJSONText

instance FromJSON RepositoryTriggerEventEnum where
  parseJSON = parseJSONText "RepositoryTriggerEventEnum"
