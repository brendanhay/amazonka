{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.ExecutionEngineType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.ExecutionEngineType where

import Network.AWS.Prelude

data ExecutionEngineType = Emr
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

instance FromText ExecutionEngineType where
  parser =
    takeLowerText >>= \case
      "emr" -> pure Emr
      e ->
        fromTextError $
          "Failure parsing ExecutionEngineType from value: '" <> e
            <> "'. Accepted values: emr"

instance ToText ExecutionEngineType where
  toText = \case
    Emr -> "EMR"

instance Hashable ExecutionEngineType

instance NFData ExecutionEngineType

instance ToByteString ExecutionEngineType

instance ToQuery ExecutionEngineType

instance ToHeader ExecutionEngineType

instance ToJSON ExecutionEngineType where
  toJSON = toJSONText

instance FromJSON ExecutionEngineType where
  parseJSON = parseJSONText "ExecutionEngineType"
