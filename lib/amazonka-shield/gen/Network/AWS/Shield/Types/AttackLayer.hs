{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.AttackLayer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.AttackLayer where

import Network.AWS.Prelude

data AttackLayer
  = Application
  | Network
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

instance FromText AttackLayer where
  parser =
    takeLowerText >>= \case
      "application" -> pure Application
      "network" -> pure Network
      e ->
        fromTextError $
          "Failure parsing AttackLayer from value: '" <> e
            <> "'. Accepted values: application, network"

instance ToText AttackLayer where
  toText = \case
    Application -> "APPLICATION"
    Network -> "NETWORK"

instance Hashable AttackLayer

instance NFData AttackLayer

instance ToByteString AttackLayer

instance ToQuery AttackLayer

instance ToHeader AttackLayer

instance FromJSON AttackLayer where
  parseJSON = parseJSONText "AttackLayer"
