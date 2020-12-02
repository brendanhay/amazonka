{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.ActionSubType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.ActionSubType where

import Network.AWS.Prelude

data ActionSubType
  = StopEC2Instances
  | StopRDSInstances
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

instance FromText ActionSubType where
  parser =
    takeLowerText >>= \case
      "stop_ec2_instances" -> pure StopEC2Instances
      "stop_rds_instances" -> pure StopRDSInstances
      e ->
        fromTextError $
          "Failure parsing ActionSubType from value: '" <> e
            <> "'. Accepted values: stop_ec2_instances, stop_rds_instances"

instance ToText ActionSubType where
  toText = \case
    StopEC2Instances -> "STOP_EC2_INSTANCES"
    StopRDSInstances -> "STOP_RDS_INSTANCES"

instance Hashable ActionSubType

instance NFData ActionSubType

instance ToByteString ActionSubType

instance ToQuery ActionSubType

instance ToHeader ActionSubType

instance ToJSON ActionSubType where
  toJSON = toJSONText

instance FromJSON ActionSubType where
  parseJSON = parseJSONText "ActionSubType"
