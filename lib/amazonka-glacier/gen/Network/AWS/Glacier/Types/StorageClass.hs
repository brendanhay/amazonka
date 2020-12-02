{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.StorageClass
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.StorageClass where

import Network.AWS.Prelude

data StorageClass
  = ReducedRedundancy
  | Standard
  | StandardIA
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

instance FromText StorageClass where
  parser =
    takeLowerText >>= \case
      "reduced_redundancy" -> pure ReducedRedundancy
      "standard" -> pure Standard
      "standard_ia" -> pure StandardIA
      e ->
        fromTextError $
          "Failure parsing StorageClass from value: '" <> e
            <> "'. Accepted values: reduced_redundancy, standard, standard_ia"

instance ToText StorageClass where
  toText = \case
    ReducedRedundancy -> "REDUCED_REDUNDANCY"
    Standard -> "STANDARD"
    StandardIA -> "STANDARD_IA"

instance Hashable StorageClass

instance NFData StorageClass

instance ToByteString StorageClass

instance ToQuery StorageClass

instance ToHeader StorageClass

instance ToJSON StorageClass where
  toJSON = toJSONText

instance FromJSON StorageClass where
  parseJSON = parseJSONText "StorageClass"
