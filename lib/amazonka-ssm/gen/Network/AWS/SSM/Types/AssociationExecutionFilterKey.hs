{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AssociationExecutionFilterKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AssociationExecutionFilterKey where

import Network.AWS.Prelude

data AssociationExecutionFilterKey
  = CreatedTime
  | ExecutionId
  | Status
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

instance FromText AssociationExecutionFilterKey where
  parser =
    takeLowerText >>= \case
      "createdtime" -> pure CreatedTime
      "executionid" -> pure ExecutionId
      "status" -> pure Status
      e ->
        fromTextError $
          "Failure parsing AssociationExecutionFilterKey from value: '" <> e
            <> "'. Accepted values: createdtime, executionid, status"

instance ToText AssociationExecutionFilterKey where
  toText = \case
    CreatedTime -> "CreatedTime"
    ExecutionId -> "ExecutionId"
    Status -> "Status"

instance Hashable AssociationExecutionFilterKey

instance NFData AssociationExecutionFilterKey

instance ToByteString AssociationExecutionFilterKey

instance ToQuery AssociationExecutionFilterKey

instance ToHeader AssociationExecutionFilterKey

instance ToJSON AssociationExecutionFilterKey where
  toJSON = toJSONText
