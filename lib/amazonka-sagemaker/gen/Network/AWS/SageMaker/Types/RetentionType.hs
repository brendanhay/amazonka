{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.RetentionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.RetentionType where

import Network.AWS.Prelude

data RetentionType
  = Delete
  | Retain
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

instance FromText RetentionType where
  parser =
    takeLowerText >>= \case
      "delete" -> pure Delete
      "retain" -> pure Retain
      e ->
        fromTextError $
          "Failure parsing RetentionType from value: '" <> e
            <> "'. Accepted values: delete, retain"

instance ToText RetentionType where
  toText = \case
    Delete -> "Delete"
    Retain -> "Retain"

instance Hashable RetentionType

instance NFData RetentionType

instance ToByteString RetentionType

instance ToQuery RetentionType

instance ToHeader RetentionType

instance ToJSON RetentionType where
  toJSON = toJSONText
