{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.RecordWrapper
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.RecordWrapper where

import Network.AWS.Prelude

data RecordWrapper
  = None
  | RecordIO
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

instance FromText RecordWrapper where
  parser =
    takeLowerText >>= \case
      "none" -> pure None
      "recordio" -> pure RecordIO
      e ->
        fromTextError $
          "Failure parsing RecordWrapper from value: '" <> e
            <> "'. Accepted values: none, recordio"

instance ToText RecordWrapper where
  toText = \case
    None -> "None"
    RecordIO -> "RecordIO"

instance Hashable RecordWrapper

instance NFData RecordWrapper

instance ToByteString RecordWrapper

instance ToQuery RecordWrapper

instance ToHeader RecordWrapper

instance ToJSON RecordWrapper where
  toJSON = toJSONText

instance FromJSON RecordWrapper where
  parseJSON = parseJSONText "RecordWrapper"
