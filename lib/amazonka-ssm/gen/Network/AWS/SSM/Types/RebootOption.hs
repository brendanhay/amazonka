{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.RebootOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.RebootOption where

import Network.AWS.Prelude

data RebootOption
  = NoReboot
  | RebootIfNeeded
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

instance FromText RebootOption where
  parser =
    takeLowerText >>= \case
      "noreboot" -> pure NoReboot
      "rebootifneeded" -> pure RebootIfNeeded
      e ->
        fromTextError $
          "Failure parsing RebootOption from value: '" <> e
            <> "'. Accepted values: noreboot, rebootifneeded"

instance ToText RebootOption where
  toText = \case
    NoReboot -> "NoReboot"
    RebootIfNeeded -> "RebootIfNeeded"

instance Hashable RebootOption

instance NFData RebootOption

instance ToByteString RebootOption

instance ToQuery RebootOption

instance ToHeader RebootOption

instance FromJSON RebootOption where
  parseJSON = parseJSONText "RebootOption"
