{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.BatchStrategy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.BatchStrategy where

import Network.AWS.Prelude

data BatchStrategy
  = MultiRecord
  | SingleRecord
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

instance FromText BatchStrategy where
  parser =
    takeLowerText >>= \case
      "multirecord" -> pure MultiRecord
      "singlerecord" -> pure SingleRecord
      e ->
        fromTextError $
          "Failure parsing BatchStrategy from value: '" <> e
            <> "'. Accepted values: multirecord, singlerecord"

instance ToText BatchStrategy where
  toText = \case
    MultiRecord -> "MultiRecord"
    SingleRecord -> "SingleRecord"

instance Hashable BatchStrategy

instance NFData BatchStrategy

instance ToByteString BatchStrategy

instance ToQuery BatchStrategy

instance ToHeader BatchStrategy

instance ToJSON BatchStrategy where
  toJSON = toJSONText

instance FromJSON BatchStrategy where
  parseJSON = parseJSONText "BatchStrategy"
