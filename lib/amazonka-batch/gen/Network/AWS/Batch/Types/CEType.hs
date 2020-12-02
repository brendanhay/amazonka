{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.CEType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.CEType where

import Network.AWS.Prelude

data CEType
  = Managed
  | Unmanaged
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

instance FromText CEType where
  parser =
    takeLowerText >>= \case
      "managed" -> pure Managed
      "unmanaged" -> pure Unmanaged
      e ->
        fromTextError $
          "Failure parsing CEType from value: '" <> e
            <> "'. Accepted values: managed, unmanaged"

instance ToText CEType where
  toText = \case
    Managed -> "MANAGED"
    Unmanaged -> "UNMANAGED"

instance Hashable CEType

instance NFData CEType

instance ToByteString CEType

instance ToQuery CEType

instance ToHeader CEType

instance ToJSON CEType where
  toJSON = toJSONText

instance FromJSON CEType where
  parseJSON = parseJSONText "CEType"
