{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.TagStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.TagStatus where

import Network.AWS.Prelude

data TagStatus
  = Any
  | Tagged
  | Untagged
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

instance FromText TagStatus where
  parser =
    takeLowerText >>= \case
      "any" -> pure Any
      "tagged" -> pure Tagged
      "untagged" -> pure Untagged
      e ->
        fromTextError $
          "Failure parsing TagStatus from value: '" <> e
            <> "'. Accepted values: any, tagged, untagged"

instance ToText TagStatus where
  toText = \case
    Any -> "ANY"
    Tagged -> "TAGGED"
    Untagged -> "UNTAGGED"

instance Hashable TagStatus

instance NFData TagStatus

instance ToByteString TagStatus

instance ToQuery TagStatus

instance ToHeader TagStatus

instance ToJSON TagStatus where
  toJSON = toJSONText
