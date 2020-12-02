{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.RedactionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.RedactionType where

import Network.AWS.Prelude

data RedactionType = Pii
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

instance FromText RedactionType where
  parser =
    takeLowerText >>= \case
      "pii" -> pure Pii
      e ->
        fromTextError $
          "Failure parsing RedactionType from value: '" <> e
            <> "'. Accepted values: pii"

instance ToText RedactionType where
  toText = \case
    Pii -> "PII"

instance Hashable RedactionType

instance NFData RedactionType

instance ToByteString RedactionType

instance ToQuery RedactionType

instance ToHeader RedactionType

instance ToJSON RedactionType where
  toJSON = toJSONText

instance FromJSON RedactionType where
  parseJSON = parseJSONText "RedactionType"
