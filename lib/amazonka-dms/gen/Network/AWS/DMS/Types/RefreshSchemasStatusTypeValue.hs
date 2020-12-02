{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.RefreshSchemasStatusTypeValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.RefreshSchemasStatusTypeValue where

import Network.AWS.Prelude

data RefreshSchemasStatusTypeValue
  = Failed
  | Refreshing
  | Successful
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

instance FromText RefreshSchemasStatusTypeValue where
  parser =
    takeLowerText >>= \case
      "failed" -> pure Failed
      "refreshing" -> pure Refreshing
      "successful" -> pure Successful
      e ->
        fromTextError $
          "Failure parsing RefreshSchemasStatusTypeValue from value: '" <> e
            <> "'. Accepted values: failed, refreshing, successful"

instance ToText RefreshSchemasStatusTypeValue where
  toText = \case
    Failed -> "failed"
    Refreshing -> "refreshing"
    Successful -> "successful"

instance Hashable RefreshSchemasStatusTypeValue

instance NFData RefreshSchemasStatusTypeValue

instance ToByteString RefreshSchemasStatusTypeValue

instance ToQuery RefreshSchemasStatusTypeValue

instance ToHeader RefreshSchemasStatusTypeValue

instance FromJSON RefreshSchemasStatusTypeValue where
  parseJSON = parseJSONText "RefreshSchemasStatusTypeValue"
