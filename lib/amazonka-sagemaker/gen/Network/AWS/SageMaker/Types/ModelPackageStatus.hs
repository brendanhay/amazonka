{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ModelPackageStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ModelPackageStatus where

import Network.AWS.Prelude

data ModelPackageStatus
  = MPSCompleted
  | MPSDeleting
  | MPSFailed
  | MPSInProgress
  | MPSPending
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

instance FromText ModelPackageStatus where
  parser =
    takeLowerText >>= \case
      "completed" -> pure MPSCompleted
      "deleting" -> pure MPSDeleting
      "failed" -> pure MPSFailed
      "inprogress" -> pure MPSInProgress
      "pending" -> pure MPSPending
      e ->
        fromTextError $
          "Failure parsing ModelPackageStatus from value: '" <> e
            <> "'. Accepted values: completed, deleting, failed, inprogress, pending"

instance ToText ModelPackageStatus where
  toText = \case
    MPSCompleted -> "Completed"
    MPSDeleting -> "Deleting"
    MPSFailed -> "Failed"
    MPSInProgress -> "InProgress"
    MPSPending -> "Pending"

instance Hashable ModelPackageStatus

instance NFData ModelPackageStatus

instance ToByteString ModelPackageStatus

instance ToQuery ModelPackageStatus

instance ToHeader ModelPackageStatus

instance FromJSON ModelPackageStatus where
  parseJSON = parseJSONText "ModelPackageStatus"
