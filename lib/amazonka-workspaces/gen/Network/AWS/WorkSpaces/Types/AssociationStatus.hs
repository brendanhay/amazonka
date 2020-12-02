{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.AssociationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.AssociationStatus where

import Network.AWS.Prelude

data AssociationStatus
  = AssociatedWithOwnerAccount
  | AssociatedWithSharedAccount
  | NotAssociated
  | PendingAssociation
  | PendingDisassociation
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

instance FromText AssociationStatus where
  parser =
    takeLowerText >>= \case
      "associated_with_owner_account" -> pure AssociatedWithOwnerAccount
      "associated_with_shared_account" -> pure AssociatedWithSharedAccount
      "not_associated" -> pure NotAssociated
      "pending_association" -> pure PendingAssociation
      "pending_disassociation" -> pure PendingDisassociation
      e ->
        fromTextError $
          "Failure parsing AssociationStatus from value: '" <> e
            <> "'. Accepted values: associated_with_owner_account, associated_with_shared_account, not_associated, pending_association, pending_disassociation"

instance ToText AssociationStatus where
  toText = \case
    AssociatedWithOwnerAccount -> "ASSOCIATED_WITH_OWNER_ACCOUNT"
    AssociatedWithSharedAccount -> "ASSOCIATED_WITH_SHARED_ACCOUNT"
    NotAssociated -> "NOT_ASSOCIATED"
    PendingAssociation -> "PENDING_ASSOCIATION"
    PendingDisassociation -> "PENDING_DISASSOCIATION"

instance Hashable AssociationStatus

instance NFData AssociationStatus

instance ToByteString AssociationStatus

instance ToQuery AssociationStatus

instance ToHeader AssociationStatus

instance FromJSON AssociationStatus where
  parseJSON = parseJSONText "AssociationStatus"
