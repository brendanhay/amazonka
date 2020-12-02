{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Language
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Language where

import Network.AWS.Prelude

data Language
  = Python
  | Scala
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

instance FromText Language where
  parser =
    takeLowerText >>= \case
      "python" -> pure Python
      "scala" -> pure Scala
      e ->
        fromTextError $
          "Failure parsing Language from value: '" <> e
            <> "'. Accepted values: python, scala"

instance ToText Language where
  toText = \case
    Python -> "PYTHON"
    Scala -> "SCALA"

instance Hashable Language

instance NFData Language

instance ToByteString Language

instance ToQuery Language

instance ToHeader Language

instance ToJSON Language where
  toJSON = toJSONText
