{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.LifecyclePolicyPreviewStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.LifecyclePolicyPreviewStatus where

import Network.AWS.Prelude

data LifecyclePolicyPreviewStatus
  = LPPSComplete
  | LPPSExpired
  | LPPSFailed
  | LPPSInProgress
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

instance FromText LifecyclePolicyPreviewStatus where
  parser =
    takeLowerText >>= \case
      "complete" -> pure LPPSComplete
      "expired" -> pure LPPSExpired
      "failed" -> pure LPPSFailed
      "in_progress" -> pure LPPSInProgress
      e ->
        fromTextError $
          "Failure parsing LifecyclePolicyPreviewStatus from value: '" <> e
            <> "'. Accepted values: complete, expired, failed, in_progress"

instance ToText LifecyclePolicyPreviewStatus where
  toText = \case
    LPPSComplete -> "COMPLETE"
    LPPSExpired -> "EXPIRED"
    LPPSFailed -> "FAILED"
    LPPSInProgress -> "IN_PROGRESS"

instance Hashable LifecyclePolicyPreviewStatus

instance NFData LifecyclePolicyPreviewStatus

instance ToByteString LifecyclePolicyPreviewStatus

instance ToQuery LifecyclePolicyPreviewStatus

instance ToHeader LifecyclePolicyPreviewStatus

instance FromJSON LifecyclePolicyPreviewStatus where
  parseJSON = parseJSONText "LifecyclePolicyPreviewStatus"
