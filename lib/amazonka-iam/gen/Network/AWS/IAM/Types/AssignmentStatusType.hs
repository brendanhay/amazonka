{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.AssignmentStatusType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.AssignmentStatusType where

import Network.AWS.Prelude

data AssignmentStatusType
  = Any
  | Assigned
  | Unassigned
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

instance FromText AssignmentStatusType where
  parser =
    takeLowerText >>= \case
      "any" -> pure Any
      "assigned" -> pure Assigned
      "unassigned" -> pure Unassigned
      e ->
        fromTextError $
          "Failure parsing AssignmentStatusType from value: '" <> e
            <> "'. Accepted values: any, assigned, unassigned"

instance ToText AssignmentStatusType where
  toText = \case
    Any -> "Any"
    Assigned -> "Assigned"
    Unassigned -> "Unassigned"

instance Hashable AssignmentStatusType

instance NFData AssignmentStatusType

instance ToByteString AssignmentStatusType

instance ToQuery AssignmentStatusType

instance ToHeader AssignmentStatusType
