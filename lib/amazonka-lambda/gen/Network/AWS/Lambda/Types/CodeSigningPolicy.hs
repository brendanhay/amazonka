{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.CodeSigningPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.CodeSigningPolicy where

import Network.AWS.Prelude

data CodeSigningPolicy
  = Enforce
  | Warn
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

instance FromText CodeSigningPolicy where
  parser =
    takeLowerText >>= \case
      "enforce" -> pure Enforce
      "warn" -> pure Warn
      e ->
        fromTextError $
          "Failure parsing CodeSigningPolicy from value: '" <> e
            <> "'. Accepted values: enforce, warn"

instance ToText CodeSigningPolicy where
  toText = \case
    Enforce -> "Enforce"
    Warn -> "Warn"

instance Hashable CodeSigningPolicy

instance NFData CodeSigningPolicy

instance ToByteString CodeSigningPolicy

instance ToQuery CodeSigningPolicy

instance ToHeader CodeSigningPolicy

instance ToJSON CodeSigningPolicy where
  toJSON = toJSONText

instance FromJSON CodeSigningPolicy where
  parseJSON = parseJSONText "CodeSigningPolicy"
