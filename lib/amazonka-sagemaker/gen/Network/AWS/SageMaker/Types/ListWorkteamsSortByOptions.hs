{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ListWorkteamsSortByOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ListWorkteamsSortByOptions where

import Network.AWS.Prelude

data ListWorkteamsSortByOptions
  = LCreateDate
  | LName
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

instance FromText ListWorkteamsSortByOptions where
  parser =
    takeLowerText >>= \case
      "createdate" -> pure LCreateDate
      "name" -> pure LName
      e ->
        fromTextError $
          "Failure parsing ListWorkteamsSortByOptions from value: '" <> e
            <> "'. Accepted values: createdate, name"

instance ToText ListWorkteamsSortByOptions where
  toText = \case
    LCreateDate -> "CreateDate"
    LName -> "Name"

instance Hashable ListWorkteamsSortByOptions

instance NFData ListWorkteamsSortByOptions

instance ToByteString ListWorkteamsSortByOptions

instance ToQuery ListWorkteamsSortByOptions

instance ToHeader ListWorkteamsSortByOptions

instance ToJSON ListWorkteamsSortByOptions where
  toJSON = toJSONText
