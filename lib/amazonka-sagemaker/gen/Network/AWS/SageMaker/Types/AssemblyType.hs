{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AssemblyType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AssemblyType where

import Network.AWS.Prelude

data AssemblyType
  = ATLine
  | ATNone
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

instance FromText AssemblyType where
  parser =
    takeLowerText >>= \case
      "line" -> pure ATLine
      "none" -> pure ATNone
      e ->
        fromTextError $
          "Failure parsing AssemblyType from value: '" <> e
            <> "'. Accepted values: line, none"

instance ToText AssemblyType where
  toText = \case
    ATLine -> "Line"
    ATNone -> "None"

instance Hashable AssemblyType

instance NFData AssemblyType

instance ToByteString AssemblyType

instance ToQuery AssemblyType

instance ToHeader AssemblyType

instance ToJSON AssemblyType where
  toJSON = toJSONText

instance FromJSON AssemblyType where
  parseJSON = parseJSONText "AssemblyType"
