{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.DocumentClassifierMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.DocumentClassifierMode where

import Network.AWS.Prelude

data DocumentClassifierMode
  = MultiClass
  | MultiLabel
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

instance FromText DocumentClassifierMode where
  parser =
    takeLowerText >>= \case
      "multi_class" -> pure MultiClass
      "multi_label" -> pure MultiLabel
      e ->
        fromTextError $
          "Failure parsing DocumentClassifierMode from value: '" <> e
            <> "'. Accepted values: multi_class, multi_label"

instance ToText DocumentClassifierMode where
  toText = \case
    MultiClass -> "MULTI_CLASS"
    MultiLabel -> "MULTI_LABEL"

instance Hashable DocumentClassifierMode

instance NFData DocumentClassifierMode

instance ToByteString DocumentClassifierMode

instance ToQuery DocumentClassifierMode

instance ToHeader DocumentClassifierMode

instance ToJSON DocumentClassifierMode where
  toJSON = toJSONText

instance FromJSON DocumentClassifierMode where
  parseJSON = parseJSONText "DocumentClassifierMode"
