{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.InputFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.InputFormat where

import Network.AWS.Prelude

data InputFormat
  = OneDocPerFile
  | OneDocPerLine
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

instance FromText InputFormat where
  parser =
    takeLowerText >>= \case
      "one_doc_per_file" -> pure OneDocPerFile
      "one_doc_per_line" -> pure OneDocPerLine
      e ->
        fromTextError $
          "Failure parsing InputFormat from value: '" <> e
            <> "'. Accepted values: one_doc_per_file, one_doc_per_line"

instance ToText InputFormat where
  toText = \case
    OneDocPerFile -> "ONE_DOC_PER_FILE"
    OneDocPerLine -> "ONE_DOC_PER_LINE"

instance Hashable InputFormat

instance NFData InputFormat

instance ToByteString InputFormat

instance ToQuery InputFormat

instance ToHeader InputFormat

instance ToJSON InputFormat where
  toJSON = toJSONText

instance FromJSON InputFormat where
  parseJSON = parseJSONText "InputFormat"
