{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.SortValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.SortValue where

import Network.AWS.Prelude

data SortValue
  = Asc
  | Desc
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

instance FromText SortValue where
  parser =
    takeLowerText >>= \case
      "asc" -> pure Asc
      "desc" -> pure Desc
      e ->
        fromTextError $
          "Failure parsing SortValue from value: '" <> e
            <> "'. Accepted values: asc, desc"

instance ToText SortValue where
  toText = \case
    Asc -> "ASC"
    Desc -> "DESC"

instance Hashable SortValue

instance NFData SortValue

instance ToByteString SortValue

instance ToQuery SortValue

instance ToHeader SortValue

instance ToJSON SortValue where
  toJSON = toJSONText
