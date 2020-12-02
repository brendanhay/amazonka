{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.FlowDefinitionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.FlowDefinitionStatus where

import Network.AWS.Prelude

data FlowDefinitionStatus
  = FDSActive
  | FDSDeleting
  | FDSFailed
  | FDSInitializing
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

instance FromText FlowDefinitionStatus where
  parser =
    takeLowerText >>= \case
      "active" -> pure FDSActive
      "deleting" -> pure FDSDeleting
      "failed" -> pure FDSFailed
      "initializing" -> pure FDSInitializing
      e ->
        fromTextError $
          "Failure parsing FlowDefinitionStatus from value: '" <> e
            <> "'. Accepted values: active, deleting, failed, initializing"

instance ToText FlowDefinitionStatus where
  toText = \case
    FDSActive -> "Active"
    FDSDeleting -> "Deleting"
    FDSFailed -> "Failed"
    FDSInitializing -> "Initializing"

instance Hashable FlowDefinitionStatus

instance NFData FlowDefinitionStatus

instance ToByteString FlowDefinitionStatus

instance ToQuery FlowDefinitionStatus

instance ToHeader FlowDefinitionStatus

instance FromJSON FlowDefinitionStatus where
  parseJSON = parseJSONText "FlowDefinitionStatus"
