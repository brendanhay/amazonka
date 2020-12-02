{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.ApprovalState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.ApprovalState where

import Network.AWS.Prelude

data ApprovalState
  = ASApprove
  | ASRevoke
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

instance FromText ApprovalState where
  parser =
    takeLowerText >>= \case
      "approve" -> pure ASApprove
      "revoke" -> pure ASRevoke
      e ->
        fromTextError $
          "Failure parsing ApprovalState from value: '" <> e
            <> "'. Accepted values: approve, revoke"

instance ToText ApprovalState where
  toText = \case
    ASApprove -> "APPROVE"
    ASRevoke -> "REVOKE"

instance Hashable ApprovalState

instance NFData ApprovalState

instance ToByteString ApprovalState

instance ToQuery ApprovalState

instance ToHeader ApprovalState

instance ToJSON ApprovalState where
  toJSON = toJSONText

instance FromJSON ApprovalState where
  parseJSON = parseJSONText "ApprovalState"
