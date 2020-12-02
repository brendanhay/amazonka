{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.ReviewPolicyLevel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.ReviewPolicyLevel where

import Network.AWS.Prelude

data ReviewPolicyLevel
  = Assignment
  | Hit
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

instance FromText ReviewPolicyLevel where
  parser =
    takeLowerText >>= \case
      "assignment" -> pure Assignment
      "hit" -> pure Hit
      e ->
        fromTextError $
          "Failure parsing ReviewPolicyLevel from value: '" <> e
            <> "'. Accepted values: assignment, hit"

instance ToText ReviewPolicyLevel where
  toText = \case
    Assignment -> "Assignment"
    Hit -> "HIT"

instance Hashable ReviewPolicyLevel

instance NFData ReviewPolicyLevel

instance ToByteString ReviewPolicyLevel

instance ToQuery ReviewPolicyLevel

instance ToHeader ReviewPolicyLevel

instance ToJSON ReviewPolicyLevel where
  toJSON = toJSONText
