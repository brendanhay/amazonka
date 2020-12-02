{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.BlueprintType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.BlueprintType where

import Network.AWS.Prelude

data BlueprintType
  = App
  | OS
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

instance FromText BlueprintType where
  parser =
    takeLowerText >>= \case
      "app" -> pure App
      "os" -> pure OS
      e ->
        fromTextError $
          "Failure parsing BlueprintType from value: '" <> e
            <> "'. Accepted values: app, os"

instance ToText BlueprintType where
  toText = \case
    App -> "app"
    OS -> "os"

instance Hashable BlueprintType

instance NFData BlueprintType

instance ToByteString BlueprintType

instance ToQuery BlueprintType

instance ToHeader BlueprintType

instance FromJSON BlueprintType where
  parseJSON = parseJSONText "BlueprintType"
