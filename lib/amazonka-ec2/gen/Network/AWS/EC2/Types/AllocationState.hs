{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AllocationState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AllocationState where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data AllocationState
  = ASAvailable
  | ASPending
  | ASPermanentFailure
  | ASReleased
  | ASReleasedPermanentFailure
  | ASUnderAssessment
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

instance FromText AllocationState where
  parser =
    takeLowerText >>= \case
      "available" -> pure ASAvailable
      "pending" -> pure ASPending
      "permanent-failure" -> pure ASPermanentFailure
      "released" -> pure ASReleased
      "released-permanent-failure" -> pure ASReleasedPermanentFailure
      "under-assessment" -> pure ASUnderAssessment
      e ->
        fromTextError $
          "Failure parsing AllocationState from value: '" <> e
            <> "'. Accepted values: available, pending, permanent-failure, released, released-permanent-failure, under-assessment"

instance ToText AllocationState where
  toText = \case
    ASAvailable -> "available"
    ASPending -> "pending"
    ASPermanentFailure -> "permanent-failure"
    ASReleased -> "released"
    ASReleasedPermanentFailure -> "released-permanent-failure"
    ASUnderAssessment -> "under-assessment"

instance Hashable AllocationState

instance NFData AllocationState

instance ToByteString AllocationState

instance ToQuery AllocationState

instance ToHeader AllocationState

instance FromXML AllocationState where
  parseXML = parseXMLText "AllocationState"
