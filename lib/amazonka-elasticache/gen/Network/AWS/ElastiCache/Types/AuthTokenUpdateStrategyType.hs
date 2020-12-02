{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.AuthTokenUpdateStrategyType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.AuthTokenUpdateStrategyType where

import Network.AWS.Prelude

data AuthTokenUpdateStrategyType
  = Delete
  | Rotate
  | Set
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

instance FromText AuthTokenUpdateStrategyType where
  parser =
    takeLowerText >>= \case
      "delete" -> pure Delete
      "rotate" -> pure Rotate
      "set" -> pure Set
      e ->
        fromTextError $
          "Failure parsing AuthTokenUpdateStrategyType from value: '" <> e
            <> "'. Accepted values: delete, rotate, set"

instance ToText AuthTokenUpdateStrategyType where
  toText = \case
    Delete -> "DELETE"
    Rotate -> "ROTATE"
    Set -> "SET"

instance Hashable AuthTokenUpdateStrategyType

instance NFData AuthTokenUpdateStrategyType

instance ToByteString AuthTokenUpdateStrategyType

instance ToQuery AuthTokenUpdateStrategyType

instance ToHeader AuthTokenUpdateStrategyType
