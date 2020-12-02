{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AssociationFilterKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AssociationFilterKey where

import Network.AWS.Prelude

data AssociationFilterKey
  = AFKAssociationId
  | AFKAssociationName
  | AFKAssociationStatusName
  | AFKInstanceId
  | AFKLastExecutedAfter
  | AFKLastExecutedBefore
  | AFKName
  | AFKResourceGroupName
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

instance FromText AssociationFilterKey where
  parser =
    takeLowerText >>= \case
      "associationid" -> pure AFKAssociationId
      "associationname" -> pure AFKAssociationName
      "associationstatusname" -> pure AFKAssociationStatusName
      "instanceid" -> pure AFKInstanceId
      "lastexecutedafter" -> pure AFKLastExecutedAfter
      "lastexecutedbefore" -> pure AFKLastExecutedBefore
      "name" -> pure AFKName
      "resourcegroupname" -> pure AFKResourceGroupName
      e ->
        fromTextError $
          "Failure parsing AssociationFilterKey from value: '" <> e
            <> "'. Accepted values: associationid, associationname, associationstatusname, instanceid, lastexecutedafter, lastexecutedbefore, name, resourcegroupname"

instance ToText AssociationFilterKey where
  toText = \case
    AFKAssociationId -> "AssociationId"
    AFKAssociationName -> "AssociationName"
    AFKAssociationStatusName -> "AssociationStatusName"
    AFKInstanceId -> "InstanceId"
    AFKLastExecutedAfter -> "LastExecutedAfter"
    AFKLastExecutedBefore -> "LastExecutedBefore"
    AFKName -> "Name"
    AFKResourceGroupName -> "ResourceGroupName"

instance Hashable AssociationFilterKey

instance NFData AssociationFilterKey

instance ToByteString AssociationFilterKey

instance ToQuery AssociationFilterKey

instance ToHeader AssociationFilterKey

instance ToJSON AssociationFilterKey where
  toJSON = toJSONText
