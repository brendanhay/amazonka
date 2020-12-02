{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.DetailedModelPackageStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.DetailedModelPackageStatus where

import Network.AWS.Prelude

data DetailedModelPackageStatus
  = DMPSCompleted
  | DMPSFailed
  | DMPSInProgress
  | DMPSNotStarted
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

instance FromText DetailedModelPackageStatus where
  parser =
    takeLowerText >>= \case
      "completed" -> pure DMPSCompleted
      "failed" -> pure DMPSFailed
      "inprogress" -> pure DMPSInProgress
      "notstarted" -> pure DMPSNotStarted
      e ->
        fromTextError $
          "Failure parsing DetailedModelPackageStatus from value: '" <> e
            <> "'. Accepted values: completed, failed, inprogress, notstarted"

instance ToText DetailedModelPackageStatus where
  toText = \case
    DMPSCompleted -> "Completed"
    DMPSFailed -> "Failed"
    DMPSInProgress -> "InProgress"
    DMPSNotStarted -> "NotStarted"

instance Hashable DetailedModelPackageStatus

instance NFData DetailedModelPackageStatus

instance ToByteString DetailedModelPackageStatus

instance ToQuery DetailedModelPackageStatus

instance ToHeader DetailedModelPackageStatus

instance FromJSON DetailedModelPackageStatus where
  parseJSON = parseJSONText "DetailedModelPackageStatus"
