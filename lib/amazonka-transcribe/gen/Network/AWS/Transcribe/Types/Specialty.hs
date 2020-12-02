{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.Specialty
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.Specialty where

import Network.AWS.Prelude

data Specialty = Primarycare
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

instance FromText Specialty where
  parser =
    takeLowerText >>= \case
      "primarycare" -> pure Primarycare
      e ->
        fromTextError $
          "Failure parsing Specialty from value: '" <> e
            <> "'. Accepted values: primarycare"

instance ToText Specialty where
  toText = \case
    Primarycare -> "PRIMARYCARE"

instance Hashable Specialty

instance NFData Specialty

instance ToByteString Specialty

instance ToQuery Specialty

instance ToHeader Specialty

instance ToJSON Specialty where
  toJSON = toJSONText

instance FromJSON Specialty where
  parseJSON = parseJSONText "Specialty"
