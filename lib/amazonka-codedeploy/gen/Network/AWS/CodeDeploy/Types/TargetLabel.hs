{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.TargetLabel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.TargetLabel where

import Network.AWS.Prelude

data TargetLabel
  = TLBlue
  | TLGreen
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

instance FromText TargetLabel where
  parser =
    takeLowerText >>= \case
      "blue" -> pure TLBlue
      "green" -> pure TLGreen
      e ->
        fromTextError $
          "Failure parsing TargetLabel from value: '" <> e
            <> "'. Accepted values: blue, green"

instance ToText TargetLabel where
  toText = \case
    TLBlue -> "Blue"
    TLGreen -> "Green"

instance Hashable TargetLabel

instance NFData TargetLabel

instance ToByteString TargetLabel

instance ToQuery TargetLabel

instance ToHeader TargetLabel

instance FromJSON TargetLabel where
  parseJSON = parseJSONText "TargetLabel"
