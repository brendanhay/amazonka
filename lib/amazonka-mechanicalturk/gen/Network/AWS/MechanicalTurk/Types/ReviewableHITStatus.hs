{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.ReviewableHITStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.ReviewableHITStatus where

import Network.AWS.Prelude

data ReviewableHITStatus
  = Reviewable
  | Reviewing
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

instance FromText ReviewableHITStatus where
  parser =
    takeLowerText >>= \case
      "reviewable" -> pure Reviewable
      "reviewing" -> pure Reviewing
      e ->
        fromTextError $
          "Failure parsing ReviewableHITStatus from value: '" <> e
            <> "'. Accepted values: reviewable, reviewing"

instance ToText ReviewableHITStatus where
  toText = \case
    Reviewable -> "Reviewable"
    Reviewing -> "Reviewing"

instance Hashable ReviewableHITStatus

instance NFData ReviewableHITStatus

instance ToByteString ReviewableHITStatus

instance ToQuery ReviewableHITStatus

instance ToHeader ReviewableHITStatus

instance ToJSON ReviewableHITStatus where
  toJSON = toJSONText
