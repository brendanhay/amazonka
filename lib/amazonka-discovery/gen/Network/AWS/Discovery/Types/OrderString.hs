{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.OrderString
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.OrderString where

import Network.AWS.Prelude

data OrderString
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

instance FromText OrderString where
  parser =
    takeLowerText >>= \case
      "asc" -> pure Asc
      "desc" -> pure Desc
      e ->
        fromTextError $
          "Failure parsing OrderString from value: '" <> e
            <> "'. Accepted values: asc, desc"

instance ToText OrderString where
  toText = \case
    Asc -> "ASC"
    Desc -> "DESC"

instance Hashable OrderString

instance NFData OrderString

instance ToByteString OrderString

instance ToQuery OrderString

instance ToHeader OrderString

instance ToJSON OrderString where
  toJSON = toJSONText
