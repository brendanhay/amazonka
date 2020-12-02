{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceGroupType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceGroupType where

import Network.AWS.Prelude

data InstanceGroupType
  = Core
  | Master
  | Task
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

instance FromText InstanceGroupType where
  parser =
    takeLowerText >>= \case
      "core" -> pure Core
      "master" -> pure Master
      "task" -> pure Task
      e ->
        fromTextError $
          "Failure parsing InstanceGroupType from value: '" <> e
            <> "'. Accepted values: core, master, task"

instance ToText InstanceGroupType where
  toText = \case
    Core -> "CORE"
    Master -> "MASTER"
    Task -> "TASK"

instance Hashable InstanceGroupType

instance NFData InstanceGroupType

instance ToByteString InstanceGroupType

instance ToQuery InstanceGroupType

instance ToHeader InstanceGroupType

instance ToJSON InstanceGroupType where
  toJSON = toJSONText

instance FromJSON InstanceGroupType where
  parseJSON = parseJSONText "InstanceGroupType"
