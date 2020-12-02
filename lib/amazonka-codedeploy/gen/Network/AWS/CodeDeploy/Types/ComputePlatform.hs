{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.ComputePlatform
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.ComputePlatform where

import Network.AWS.Prelude

data ComputePlatform
  = Ecs
  | Lambda
  | Server
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

instance FromText ComputePlatform where
  parser =
    takeLowerText >>= \case
      "ecs" -> pure Ecs
      "lambda" -> pure Lambda
      "server" -> pure Server
      e ->
        fromTextError $
          "Failure parsing ComputePlatform from value: '" <> e
            <> "'. Accepted values: ecs, lambda, server"

instance ToText ComputePlatform where
  toText = \case
    Ecs -> "ECS"
    Lambda -> "Lambda"
    Server -> "Server"

instance Hashable ComputePlatform

instance NFData ComputePlatform

instance ToByteString ComputePlatform

instance ToQuery ComputePlatform

instance ToHeader ComputePlatform

instance ToJSON ComputePlatform where
  toJSON = toJSONText

instance FromJSON ComputePlatform where
  parseJSON = parseJSONText "ComputePlatform"
