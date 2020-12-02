{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.CancelStepsRequestStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.CancelStepsRequestStatus where

import Network.AWS.Prelude

data CancelStepsRequestStatus
  = CSRSFailed
  | CSRSSubmitted
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

instance FromText CancelStepsRequestStatus where
  parser =
    takeLowerText >>= \case
      "failed" -> pure CSRSFailed
      "submitted" -> pure CSRSSubmitted
      e ->
        fromTextError $
          "Failure parsing CancelStepsRequestStatus from value: '" <> e
            <> "'. Accepted values: failed, submitted"

instance ToText CancelStepsRequestStatus where
  toText = \case
    CSRSFailed -> "FAILED"
    CSRSSubmitted -> "SUBMITTED"

instance Hashable CancelStepsRequestStatus

instance NFData CancelStepsRequestStatus

instance ToByteString CancelStepsRequestStatus

instance ToQuery CancelStepsRequestStatus

instance ToHeader CancelStepsRequestStatus

instance FromJSON CancelStepsRequestStatus where
  parseJSON = parseJSONText "CancelStepsRequestStatus"
