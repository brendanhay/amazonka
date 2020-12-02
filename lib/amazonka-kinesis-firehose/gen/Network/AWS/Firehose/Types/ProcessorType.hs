{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.ProcessorType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.ProcessorType where

import Network.AWS.Prelude

data ProcessorType = Lambda
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

instance FromText ProcessorType where
  parser =
    takeLowerText >>= \case
      "lambda" -> pure Lambda
      e ->
        fromTextError $
          "Failure parsing ProcessorType from value: '" <> e
            <> "'. Accepted values: lambda"

instance ToText ProcessorType where
  toText = \case
    Lambda -> "Lambda"

instance Hashable ProcessorType

instance NFData ProcessorType

instance ToByteString ProcessorType

instance ToQuery ProcessorType

instance ToHeader ProcessorType

instance ToJSON ProcessorType where
  toJSON = toJSONText

instance FromJSON ProcessorType where
  parseJSON = parseJSONText "ProcessorType"
