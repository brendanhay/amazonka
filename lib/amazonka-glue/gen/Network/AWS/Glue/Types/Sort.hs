{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Sort
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Sort where

import Network.AWS.Prelude

data Sort
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

instance FromText Sort where
  parser =
    takeLowerText >>= \case
      "asc" -> pure Asc
      "desc" -> pure Desc
      e ->
        fromTextError $
          "Failure parsing Sort from value: '" <> e
            <> "'. Accepted values: asc, desc"

instance ToText Sort where
  toText = \case
    Asc -> "ASC"
    Desc -> "DESC"

instance Hashable Sort

instance NFData Sort

instance ToByteString Sort

instance ToQuery Sort

instance ToHeader Sort

instance ToJSON Sort where
  toJSON = toJSONText
