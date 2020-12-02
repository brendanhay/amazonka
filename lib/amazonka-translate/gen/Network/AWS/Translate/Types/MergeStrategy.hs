{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.Types.MergeStrategy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Translate.Types.MergeStrategy where

import Network.AWS.Prelude

data MergeStrategy = Overwrite
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

instance FromText MergeStrategy where
  parser =
    takeLowerText >>= \case
      "overwrite" -> pure Overwrite
      e ->
        fromTextError $
          "Failure parsing MergeStrategy from value: '" <> e
            <> "'. Accepted values: overwrite"

instance ToText MergeStrategy where
  toText = \case
    Overwrite -> "OVERWRITE"

instance Hashable MergeStrategy

instance NFData MergeStrategy

instance ToByteString MergeStrategy

instance ToQuery MergeStrategy

instance ToHeader MergeStrategy

instance ToJSON MergeStrategy where
  toJSON = toJSONText
