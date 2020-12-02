{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.VocabularyState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.VocabularyState where

import Network.AWS.Prelude

data VocabularyState
  = Failed
  | Pending
  | Ready
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

instance FromText VocabularyState where
  parser =
    takeLowerText >>= \case
      "failed" -> pure Failed
      "pending" -> pure Pending
      "ready" -> pure Ready
      e ->
        fromTextError $
          "Failure parsing VocabularyState from value: '" <> e
            <> "'. Accepted values: failed, pending, ready"

instance ToText VocabularyState where
  toText = \case
    Failed -> "FAILED"
    Pending -> "PENDING"
    Ready -> "READY"

instance Hashable VocabularyState

instance NFData VocabularyState

instance ToByteString VocabularyState

instance ToQuery VocabularyState

instance ToHeader VocabularyState

instance ToJSON VocabularyState where
  toJSON = toJSONText

instance FromJSON VocabularyState where
  parseJSON = parseJSONText "VocabularyState"
