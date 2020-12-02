{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.ScanStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.ScanStatus where

import Network.AWS.Prelude

data ScanStatus
  = Complete
  | Failed
  | InProgress
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

instance FromText ScanStatus where
  parser =
    takeLowerText >>= \case
      "complete" -> pure Complete
      "failed" -> pure Failed
      "in_progress" -> pure InProgress
      e ->
        fromTextError $
          "Failure parsing ScanStatus from value: '" <> e
            <> "'. Accepted values: complete, failed, in_progress"

instance ToText ScanStatus where
  toText = \case
    Complete -> "COMPLETE"
    Failed -> "FAILED"
    InProgress -> "IN_PROGRESS"

instance Hashable ScanStatus

instance NFData ScanStatus

instance ToByteString ScanStatus

instance ToQuery ScanStatus

instance ToHeader ScanStatus

instance FromJSON ScanStatus where
  parseJSON = parseJSONText "ScanStatus"
