{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.RedactionOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.RedactionOutput where

import Network.AWS.Prelude

data RedactionOutput
  = Redacted
  | RedactedAndUnredacted
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

instance FromText RedactionOutput where
  parser =
    takeLowerText >>= \case
      "redacted" -> pure Redacted
      "redacted_and_unredacted" -> pure RedactedAndUnredacted
      e ->
        fromTextError $
          "Failure parsing RedactionOutput from value: '" <> e
            <> "'. Accepted values: redacted, redacted_and_unredacted"

instance ToText RedactionOutput where
  toText = \case
    Redacted -> "redacted"
    RedactedAndUnredacted -> "redacted_and_unredacted"

instance Hashable RedactionOutput

instance NFData RedactionOutput

instance ToByteString RedactionOutput

instance ToQuery RedactionOutput

instance ToHeader RedactionOutput

instance ToJSON RedactionOutput where
  toJSON = toJSONText

instance FromJSON RedactionOutput where
  parseJSON = parseJSONText "RedactionOutput"
