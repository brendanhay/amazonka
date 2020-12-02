{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.OperationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.OperationStatus where

import Network.AWS.Prelude

data OperationStatus
  = Fail
  | Pending
  | Submitted
  | Success
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
      "fail" -> pure Fail
      "pending" -> pure Pending
      "submitted" -> pure Submitted
      "success" -> pure Success
      e ->
        fromTextError $
          "Failure parsing OperationStatus from value: '" <> e
            <> "'. Accepted values: fail, pending, submitted, success"

instance ToText OperationStatus where
  toText = \case
    Fail -> "FAIL"
    Pending -> "PENDING"
    Submitted -> "SUBMITTED"
    Success -> "SUCCESS"

instance Hashable OperationStatus

instance NFData OperationStatus

instance ToByteString OperationStatus

instance ToQuery OperationStatus

instance ToHeader OperationStatus

instance FromJSON OperationStatus where
  parseJSON = parseJSONText "OperationStatus"
