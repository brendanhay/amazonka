{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.EventType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.EventType where

import Network.AWS.Prelude

data EventType
  = CaCertificate
  | Certificate
  | Job
  | JobExecution
  | Policy
  | Thing
  | ThingGroup
  | ThingGroupHierarchy
  | ThingGroupMembership
  | ThingType
  | ThingTypeAssociation
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

instance FromText EventType where
  parser =
    takeLowerText >>= \case
      "ca_certificate" -> pure CaCertificate
      "certificate" -> pure Certificate
      "job" -> pure Job
      "job_execution" -> pure JobExecution
      "policy" -> pure Policy
      "thing" -> pure Thing
      "thing_group" -> pure ThingGroup
      "thing_group_hierarchy" -> pure ThingGroupHierarchy
      "thing_group_membership" -> pure ThingGroupMembership
      "thing_type" -> pure ThingType
      "thing_type_association" -> pure ThingTypeAssociation
      e ->
        fromTextError $
          "Failure parsing EventType from value: '" <> e
            <> "'. Accepted values: ca_certificate, certificate, job, job_execution, policy, thing, thing_group, thing_group_hierarchy, thing_group_membership, thing_type, thing_type_association"

instance ToText EventType where
  toText = \case
    CaCertificate -> "CA_CERTIFICATE"
    Certificate -> "CERTIFICATE"
    Job -> "JOB"
    JobExecution -> "JOB_EXECUTION"
    Policy -> "POLICY"
    Thing -> "THING"
    ThingGroup -> "THING_GROUP"
    ThingGroupHierarchy -> "THING_GROUP_HIERARCHY"
    ThingGroupMembership -> "THING_GROUP_MEMBERSHIP"
    ThingType -> "THING_TYPE"
    ThingTypeAssociation -> "THING_TYPE_ASSOCIATION"

instance Hashable EventType

instance NFData EventType

instance ToByteString EventType

instance ToQuery EventType

instance ToHeader EventType

instance ToJSON EventType where
  toJSON = toJSONText

instance FromJSON EventType where
  parseJSON = parseJSONText "EventType"
