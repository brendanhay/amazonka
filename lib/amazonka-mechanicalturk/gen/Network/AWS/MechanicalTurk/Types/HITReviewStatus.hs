{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.HITReviewStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.HITReviewStatus where

import Network.AWS.Prelude

data HITReviewStatus
  = MarkedForReview
  | NotReviewed
  | ReviewedAppropriate
  | ReviewedInappropriate
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

instance FromText HITReviewStatus where
  parser =
    takeLowerText >>= \case
      "markedforreview" -> pure MarkedForReview
      "notreviewed" -> pure NotReviewed
      "reviewedappropriate" -> pure ReviewedAppropriate
      "reviewedinappropriate" -> pure ReviewedInappropriate
      e ->
        fromTextError $
          "Failure parsing HITReviewStatus from value: '" <> e
            <> "'. Accepted values: markedforreview, notreviewed, reviewedappropriate, reviewedinappropriate"

instance ToText HITReviewStatus where
  toText = \case
    MarkedForReview -> "MarkedForReview"
    NotReviewed -> "NotReviewed"
    ReviewedAppropriate -> "ReviewedAppropriate"
    ReviewedInappropriate -> "ReviewedInappropriate"

instance Hashable HITReviewStatus

instance NFData HITReviewStatus

instance ToByteString HITReviewStatus

instance ToQuery HITReviewStatus

instance ToHeader HITReviewStatus

instance FromJSON HITReviewStatus where
  parseJSON = parseJSONText "HITReviewStatus"
