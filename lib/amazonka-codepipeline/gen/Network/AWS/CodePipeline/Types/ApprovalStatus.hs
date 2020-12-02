{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ApprovalStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ApprovalStatus where

import Network.AWS.Prelude

data ApprovalStatus
  = Approved
  | Rejected
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

instance FromText ApprovalStatus where
  parser =
    takeLowerText >>= \case
      "approved" -> pure Approved
      "rejected" -> pure Rejected
      e ->
        fromTextError $
          "Failure parsing ApprovalStatus from value: '" <> e
            <> "'. Accepted values: approved, rejected"

instance ToText ApprovalStatus where
  toText = \case
    Approved -> "Approved"
    Rejected -> "Rejected"

instance Hashable ApprovalStatus

instance NFData ApprovalStatus

instance ToByteString ApprovalStatus

instance ToQuery ApprovalStatus

instance ToHeader ApprovalStatus

instance ToJSON ApprovalStatus where
  toJSON = toJSONText
