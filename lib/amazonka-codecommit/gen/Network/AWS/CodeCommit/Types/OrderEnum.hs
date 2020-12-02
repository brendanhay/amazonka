{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.OrderEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.OrderEnum where

import Network.AWS.Prelude

data OrderEnum
  = Ascending
  | Descending
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

instance FromText OrderEnum where
  parser =
    takeLowerText >>= \case
      "ascending" -> pure Ascending
      "descending" -> pure Descending
      e ->
        fromTextError $
          "Failure parsing OrderEnum from value: '" <> e
            <> "'. Accepted values: ascending, descending"

instance ToText OrderEnum where
  toText = \case
    Ascending -> "ascending"
    Descending -> "descending"

instance Hashable OrderEnum

instance NFData OrderEnum

instance ToByteString OrderEnum

instance ToQuery OrderEnum

instance ToHeader OrderEnum

instance ToJSON OrderEnum where
  toJSON = toJSONText
