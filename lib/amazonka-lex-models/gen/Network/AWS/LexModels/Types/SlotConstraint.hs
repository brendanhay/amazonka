{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.SlotConstraint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.SlotConstraint where

import Network.AWS.Prelude

data SlotConstraint
  = Optional
  | Required
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

instance FromText SlotConstraint where
  parser =
    takeLowerText >>= \case
      "optional" -> pure Optional
      "required" -> pure Required
      e ->
        fromTextError $
          "Failure parsing SlotConstraint from value: '" <> e
            <> "'. Accepted values: optional, required"

instance ToText SlotConstraint where
  toText = \case
    Optional -> "Optional"
    Required -> "Required"

instance Hashable SlotConstraint

instance NFData SlotConstraint

instance ToByteString SlotConstraint

instance ToQuery SlotConstraint

instance ToHeader SlotConstraint

instance ToJSON SlotConstraint where
  toJSON = toJSONText

instance FromJSON SlotConstraint where
  parseJSON = parseJSONText "SlotConstraint"
