{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.GenderType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.GenderType where

import Network.AWS.Prelude

data GenderType
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

instance FromText GenderType where
  parser =
    takeLowerText >>= \case
      "female" -> pure Female
      "male" -> pure Male
      e ->
        fromTextError $
          "Failure parsing GenderType from value: '" <> e
            <> "'. Accepted values: female, male"

instance ToText GenderType where
  toText = \case
    Female -> "Female"
    Male -> "Male"

instance Hashable GenderType

instance NFData GenderType

instance ToByteString GenderType

instance ToQuery GenderType

instance ToHeader GenderType

instance FromJSON GenderType where
  parseJSON = parseJSONText "GenderType"
