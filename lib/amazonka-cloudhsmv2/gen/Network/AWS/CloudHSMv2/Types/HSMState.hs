{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.Types.HSMState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudHSMv2.Types.HSMState where

import Network.AWS.Prelude

data HSMState
  = HSActive
  | HSCreateInProgress
  | HSDegraded
  | HSDeleteInProgress
  | HSDeleted
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

instance FromText HSMState where
  parser =
    takeLowerText >>= \case
      "active" -> pure HSActive
      "create_in_progress" -> pure HSCreateInProgress
      "degraded" -> pure HSDegraded
      "delete_in_progress" -> pure HSDeleteInProgress
      "deleted" -> pure HSDeleted
      e ->
        fromTextError $
          "Failure parsing HSMState from value: '" <> e
            <> "'. Accepted values: active, create_in_progress, degraded, delete_in_progress, deleted"

instance ToText HSMState where
  toText = \case
    HSActive -> "ACTIVE"
    HSCreateInProgress -> "CREATE_IN_PROGRESS"
    HSDegraded -> "DEGRADED"
    HSDeleteInProgress -> "DELETE_IN_PROGRESS"
    HSDeleted -> "DELETED"

instance Hashable HSMState

instance NFData HSMState

instance ToByteString HSMState

instance ToQuery HSMState

instance ToHeader HSMState

instance FromJSON HSMState where
  parseJSON = parseJSONText "HSMState"
