{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AssociationExecutionTargetsFilterKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AssociationExecutionTargetsFilterKey where

import Network.AWS.Prelude

data AssociationExecutionTargetsFilterKey
  = AETFKResourceId
  | AETFKResourceType
  | AETFKStatus
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

instance FromText AssociationExecutionTargetsFilterKey where
  parser =
    takeLowerText >>= \case
      "resourceid" -> pure AETFKResourceId
      "resourcetype" -> pure AETFKResourceType
      "status" -> pure AETFKStatus
      e ->
        fromTextError $
          "Failure parsing AssociationExecutionTargetsFilterKey from value: '" <> e
            <> "'. Accepted values: resourceid, resourcetype, status"

instance ToText AssociationExecutionTargetsFilterKey where
  toText = \case
    AETFKResourceId -> "ResourceId"
    AETFKResourceType -> "ResourceType"
    AETFKStatus -> "Status"

instance Hashable AssociationExecutionTargetsFilterKey

instance NFData AssociationExecutionTargetsFilterKey

instance ToByteString AssociationExecutionTargetsFilterKey

instance ToQuery AssociationExecutionTargetsFilterKey

instance ToHeader AssociationExecutionTargetsFilterKey

instance ToJSON AssociationExecutionTargetsFilterKey where
  toJSON = toJSONText
