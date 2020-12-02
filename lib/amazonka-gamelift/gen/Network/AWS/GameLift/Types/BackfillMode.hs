{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.BackfillMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.BackfillMode where

import Network.AWS.Prelude

data BackfillMode
  = Automatic
  | Manual
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

instance FromText BackfillMode where
  parser =
    takeLowerText >>= \case
      "automatic" -> pure Automatic
      "manual" -> pure Manual
      e ->
        fromTextError $
          "Failure parsing BackfillMode from value: '" <> e
            <> "'. Accepted values: automatic, manual"

instance ToText BackfillMode where
  toText = \case
    Automatic -> "AUTOMATIC"
    Manual -> "MANUAL"

instance Hashable BackfillMode

instance NFData BackfillMode

instance ToByteString BackfillMode

instance ToQuery BackfillMode

instance ToHeader BackfillMode

instance ToJSON BackfillMode where
  toJSON = toJSONText

instance FromJSON BackfillMode where
  parseJSON = parseJSONText "BackfillMode"
