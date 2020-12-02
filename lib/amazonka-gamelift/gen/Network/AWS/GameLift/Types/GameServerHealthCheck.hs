{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.GameServerHealthCheck
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.GameServerHealthCheck where

import Network.AWS.Prelude

data GameServerHealthCheck = Healthy
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

instance FromText GameServerHealthCheck where
  parser =
    takeLowerText >>= \case
      "healthy" -> pure Healthy
      e ->
        fromTextError $
          "Failure parsing GameServerHealthCheck from value: '" <> e
            <> "'. Accepted values: healthy"

instance ToText GameServerHealthCheck where
  toText = \case
    Healthy -> "HEALTHY"

instance Hashable GameServerHealthCheck

instance NFData GameServerHealthCheck

instance ToByteString GameServerHealthCheck

instance ToQuery GameServerHealthCheck

instance ToHeader GameServerHealthCheck

instance ToJSON GameServerHealthCheck where
  toJSON = toJSONText
