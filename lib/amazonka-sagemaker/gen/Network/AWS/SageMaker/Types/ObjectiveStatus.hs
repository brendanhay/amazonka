{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ObjectiveStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ObjectiveStatus where

import Network.AWS.Prelude

data ObjectiveStatus
  = OSFailed
  | OSPending
  | OSSucceeded
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

instance FromText ObjectiveStatus where
  parser =
    takeLowerText >>= \case
      "failed" -> pure OSFailed
      "pending" -> pure OSPending
      "succeeded" -> pure OSSucceeded
      e ->
        fromTextError $
          "Failure parsing ObjectiveStatus from value: '" <> e
            <> "'. Accepted values: failed, pending, succeeded"

instance ToText ObjectiveStatus where
  toText = \case
    OSFailed -> "Failed"
    OSPending -> "Pending"
    OSSucceeded -> "Succeeded"

instance Hashable ObjectiveStatus

instance NFData ObjectiveStatus

instance ToByteString ObjectiveStatus

instance ToQuery ObjectiveStatus

instance ToHeader ObjectiveStatus

instance FromJSON ObjectiveStatus where
  parseJSON = parseJSONText "ObjectiveStatus"
