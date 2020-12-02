{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.ImageTagMutability
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.ImageTagMutability where

import Network.AWS.Prelude

data ImageTagMutability
  = Immutable
  | Mutable
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

instance FromText ImageTagMutability where
  parser =
    takeLowerText >>= \case
      "immutable" -> pure Immutable
      "mutable" -> pure Mutable
      e ->
        fromTextError $
          "Failure parsing ImageTagMutability from value: '" <> e
            <> "'. Accepted values: immutable, mutable"

instance ToText ImageTagMutability where
  toText = \case
    Immutable -> "IMMUTABLE"
    Mutable -> "MUTABLE"

instance Hashable ImageTagMutability

instance NFData ImageTagMutability

instance ToByteString ImageTagMutability

instance ToQuery ImageTagMutability

instance ToHeader ImageTagMutability

instance ToJSON ImageTagMutability where
  toJSON = toJSONText

instance FromJSON ImageTagMutability where
  parseJSON = parseJSONText "ImageTagMutability"
