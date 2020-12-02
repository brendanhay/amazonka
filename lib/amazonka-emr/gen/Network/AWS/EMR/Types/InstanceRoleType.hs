{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceRoleType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceRoleType where

import Network.AWS.Prelude

data InstanceRoleType
  = IRTCore
  | IRTMaster
  | IRTTask
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

instance FromText InstanceRoleType where
  parser =
    takeLowerText >>= \case
      "core" -> pure IRTCore
      "master" -> pure IRTMaster
      "task" -> pure IRTTask
      e ->
        fromTextError $
          "Failure parsing InstanceRoleType from value: '" <> e
            <> "'. Accepted values: core, master, task"

instance ToText InstanceRoleType where
  toText = \case
    IRTCore -> "CORE"
    IRTMaster -> "MASTER"
    IRTTask -> "TASK"

instance Hashable InstanceRoleType

instance NFData InstanceRoleType

instance ToByteString InstanceRoleType

instance ToQuery InstanceRoleType

instance ToHeader InstanceRoleType

instance ToJSON InstanceRoleType where
  toJSON = toJSONText

instance FromJSON InstanceRoleType where
  parseJSON = parseJSONText "InstanceRoleType"
