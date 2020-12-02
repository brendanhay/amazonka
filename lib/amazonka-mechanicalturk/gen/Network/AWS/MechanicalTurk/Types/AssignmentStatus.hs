{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.AssignmentStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.AssignmentStatus where

import Network.AWS.Prelude

data AssignmentStatus
  = Approved
  | Rejected
  | Submitted
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

instance FromText AssignmentStatus where
  parser =
    takeLowerText >>= \case
      "approved" -> pure Approved
      "rejected" -> pure Rejected
      "submitted" -> pure Submitted
      e ->
        fromTextError $
          "Failure parsing AssignmentStatus from value: '" <> e
            <> "'. Accepted values: approved, rejected, submitted"

instance ToText AssignmentStatus where
  toText = \case
    Approved -> "Approved"
    Rejected -> "Rejected"
    Submitted -> "Submitted"

instance Hashable AssignmentStatus

instance NFData AssignmentStatus

instance ToByteString AssignmentStatus

instance ToQuery AssignmentStatus

instance ToHeader AssignmentStatus

instance ToJSON AssignmentStatus where
  toJSON = toJSONText

instance FromJSON AssignmentStatus where
  parseJSON = parseJSONText "AssignmentStatus"
