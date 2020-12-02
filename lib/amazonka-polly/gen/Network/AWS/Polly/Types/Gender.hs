{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Polly.Types.Gender
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Polly.Types.Gender where

import Network.AWS.Prelude

data Gender
  = Female
  | Male
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

instance FromText Gender where
  parser =
    takeLowerText >>= \case
      "female" -> pure Female
      "male" -> pure Male
      e ->
        fromTextError $
          "Failure parsing Gender from value: '" <> e
            <> "'. Accepted values: female, male"

instance ToText Gender where
  toText = \case
    Female -> "Female"
    Male -> "Male"

instance Hashable Gender

instance NFData Gender

instance ToByteString Gender

instance ToQuery Gender

instance ToHeader Gender

instance FromJSON Gender where
  parseJSON = parseJSONText "Gender"
