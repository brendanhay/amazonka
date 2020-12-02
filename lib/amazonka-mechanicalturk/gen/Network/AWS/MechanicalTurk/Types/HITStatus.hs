{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.HITStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.HITStatus where

import Network.AWS.Prelude

data HITStatus
  = HITSAssignable
  | HITSDisposed
  | HITSReviewable
  | HITSReviewing
  | HITSUnassignable
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

instance FromText HITStatus where
  parser =
    takeLowerText >>= \case
      "assignable" -> pure HITSAssignable
      "disposed" -> pure HITSDisposed
      "reviewable" -> pure HITSReviewable
      "reviewing" -> pure HITSReviewing
      "unassignable" -> pure HITSUnassignable
      e ->
        fromTextError $
          "Failure parsing HITStatus from value: '" <> e
            <> "'. Accepted values: assignable, disposed, reviewable, reviewing, unassignable"

instance ToText HITStatus where
  toText = \case
    HITSAssignable -> "Assignable"
    HITSDisposed -> "Disposed"
    HITSReviewable -> "Reviewable"
    HITSReviewing -> "Reviewing"
    HITSUnassignable -> "Unassignable"

instance Hashable HITStatus

instance NFData HITStatus

instance ToByteString HITStatus

instance ToQuery HITStatus

instance ToHeader HITStatus

instance FromJSON HITStatus where
  parseJSON = parseJSONText "HITStatus"
