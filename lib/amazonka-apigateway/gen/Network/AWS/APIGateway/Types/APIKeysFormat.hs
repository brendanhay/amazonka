{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.APIKeysFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.APIKeysFormat where

import Network.AWS.Prelude

data APIKeysFormat = CSV
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

instance FromText APIKeysFormat where
  parser =
    takeLowerText >>= \case
      "csv" -> pure CSV
      e ->
        fromTextError $
          "Failure parsing APIKeysFormat from value: '" <> e
            <> "'. Accepted values: csv"

instance ToText APIKeysFormat where
  toText = \case
    CSV -> "csv"

instance Hashable APIKeysFormat

instance NFData APIKeysFormat

instance ToByteString APIKeysFormat

instance ToQuery APIKeysFormat

instance ToHeader APIKeysFormat

instance ToJSON APIKeysFormat where
  toJSON = toJSONText
