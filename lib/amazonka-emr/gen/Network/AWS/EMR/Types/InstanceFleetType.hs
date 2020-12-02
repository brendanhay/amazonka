{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceFleetType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceFleetType where

import Network.AWS.Prelude

data InstanceFleetType
  = IFTCore
  | IFTMaster
  | IFTTask
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

instance FromText InstanceFleetType where
  parser =
    takeLowerText >>= \case
      "core" -> pure IFTCore
      "master" -> pure IFTMaster
      "task" -> pure IFTTask
      e ->
        fromTextError $
          "Failure parsing InstanceFleetType from value: '" <> e
            <> "'. Accepted values: core, master, task"

instance ToText InstanceFleetType where
  toText = \case
    IFTCore -> "CORE"
    IFTMaster -> "MASTER"
    IFTTask -> "TASK"

instance Hashable InstanceFleetType

instance NFData InstanceFleetType

instance ToByteString InstanceFleetType

instance ToQuery InstanceFleetType

instance ToHeader InstanceFleetType

instance ToJSON InstanceFleetType where
  toJSON = toJSONText

instance FromJSON InstanceFleetType where
  parseJSON = parseJSONText "InstanceFleetType"
