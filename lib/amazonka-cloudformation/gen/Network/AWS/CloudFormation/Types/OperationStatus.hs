{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.OperationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.OperationStatus where

import Network.AWS.Prelude

data OperationStatus
  = OSFailed
  | OSInProgress
  | OSPending
  | OSSuccess
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

instance FromText OperationStatus where
  parser =
    takeLowerText >>= \case
      "failed" -> pure OSFailed
      "in_progress" -> pure OSInProgress
      "pending" -> pure OSPending
      "success" -> pure OSSuccess
      e ->
        fromTextError $
          "Failure parsing OperationStatus from value: '" <> e
            <> "'. Accepted values: failed, in_progress, pending, success"

instance ToText OperationStatus where
  toText = \case
    OSFailed -> "FAILED"
    OSInProgress -> "IN_PROGRESS"
    OSPending -> "PENDING"
    OSSuccess -> "SUCCESS"

instance Hashable OperationStatus

instance NFData OperationStatus

instance ToByteString OperationStatus

instance ToQuery OperationStatus

instance ToHeader OperationStatus
