{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TransformStatusType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TransformStatusType where

import Network.AWS.Prelude

data TransformStatusType
  = Deleting
  | NotReady
  | Ready
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

instance FromText TransformStatusType where
  parser =
    takeLowerText >>= \case
      "deleting" -> pure Deleting
      "not_ready" -> pure NotReady
      "ready" -> pure Ready
      e ->
        fromTextError $
          "Failure parsing TransformStatusType from value: '" <> e
            <> "'. Accepted values: deleting, not_ready, ready"

instance ToText TransformStatusType where
  toText = \case
    Deleting -> "DELETING"
    NotReady -> "NOT_READY"
    Ready -> "READY"

instance Hashable TransformStatusType

instance NFData TransformStatusType

instance ToByteString TransformStatusType

instance ToQuery TransformStatusType

instance ToHeader TransformStatusType

instance ToJSON TransformStatusType where
  toJSON = toJSONText

instance FromJSON TransformStatusType where
  parseJSON = parseJSONText "TransformStatusType"
