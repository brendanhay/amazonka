{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.PatchDeploymentStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.PatchDeploymentStatus where

import Network.AWS.Prelude

data PatchDeploymentStatus
  = Approved
  | ExplicitApproved
  | ExplicitRejected
  | PendingApproval
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

instance FromText PatchDeploymentStatus where
  parser =
    takeLowerText >>= \case
      "approved" -> pure Approved
      "explicit_approved" -> pure ExplicitApproved
      "explicit_rejected" -> pure ExplicitRejected
      "pending_approval" -> pure PendingApproval
      e ->
        fromTextError $
          "Failure parsing PatchDeploymentStatus from value: '" <> e
            <> "'. Accepted values: approved, explicit_approved, explicit_rejected, pending_approval"

instance ToText PatchDeploymentStatus where
  toText = \case
    Approved -> "APPROVED"
    ExplicitApproved -> "EXPLICIT_APPROVED"
    ExplicitRejected -> "EXPLICIT_REJECTED"
    PendingApproval -> "PENDING_APPROVAL"

instance Hashable PatchDeploymentStatus

instance NFData PatchDeploymentStatus

instance ToByteString PatchDeploymentStatus

instance ToQuery PatchDeploymentStatus

instance ToHeader PatchDeploymentStatus

instance FromJSON PatchDeploymentStatus where
  parseJSON = parseJSONText "PatchDeploymentStatus"
