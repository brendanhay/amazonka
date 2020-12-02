{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Commitment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Commitment where

import Network.AWS.Prelude

-- | The length of the term of your reserved queue pricing plan commitment.
data Commitment = OneYear
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

instance FromText Commitment where
  parser =
    takeLowerText >>= \case
      "one_year" -> pure OneYear
      e ->
        fromTextError $
          "Failure parsing Commitment from value: '" <> e
            <> "'. Accepted values: one_year"

instance ToText Commitment where
  toText = \case
    OneYear -> "ONE_YEAR"

instance Hashable Commitment

instance NFData Commitment

instance ToByteString Commitment

instance ToQuery Commitment

instance ToHeader Commitment

instance ToJSON Commitment where
  toJSON = toJSONText

instance FromJSON Commitment where
  parseJSON = parseJSONText "Commitment"
