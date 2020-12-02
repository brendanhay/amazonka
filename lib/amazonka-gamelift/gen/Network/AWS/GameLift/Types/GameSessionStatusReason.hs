{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.GameSessionStatusReason
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.GameSessionStatusReason where

import Network.AWS.Prelude

data GameSessionStatusReason = Interrupted
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

instance FromText GameSessionStatusReason where
  parser =
    takeLowerText >>= \case
      "interrupted" -> pure Interrupted
      e ->
        fromTextError $
          "Failure parsing GameSessionStatusReason from value: '" <> e
            <> "'. Accepted values: interrupted"

instance ToText GameSessionStatusReason where
  toText = \case
    Interrupted -> "INTERRUPTED"

instance Hashable GameSessionStatusReason

instance NFData GameSessionStatusReason

instance ToByteString GameSessionStatusReason

instance ToQuery GameSessionStatusReason

instance ToHeader GameSessionStatusReason

instance FromJSON GameSessionStatusReason where
  parseJSON = parseJSONText "GameSessionStatusReason"
