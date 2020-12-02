{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.SplitType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.SplitType where

import Network.AWS.Prelude

data SplitType
  = STLine
  | STNone
  | STRecordIO
  | STTFRecord
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

instance FromText SplitType where
  parser =
    takeLowerText >>= \case
      "line" -> pure STLine
      "none" -> pure STNone
      "recordio" -> pure STRecordIO
      "tfrecord" -> pure STTFRecord
      e ->
        fromTextError $
          "Failure parsing SplitType from value: '" <> e
            <> "'. Accepted values: line, none, recordio, tfrecord"

instance ToText SplitType where
  toText = \case
    STLine -> "Line"
    STNone -> "None"
    STRecordIO -> "RecordIO"
    STTFRecord -> "TFRecord"

instance Hashable SplitType

instance NFData SplitType

instance ToByteString SplitType

instance ToQuery SplitType

instance ToHeader SplitType

instance ToJSON SplitType where
  toJSON = toJSONText

instance FromJSON SplitType where
  parseJSON = parseJSONText "SplitType"
