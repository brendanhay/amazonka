{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.Compatibility
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.Compatibility where

import Network.AWS.Prelude

data Compatibility
  = CEC2
  | CFargate
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

instance FromText Compatibility where
  parser =
    takeLowerText >>= \case
      "ec2" -> pure CEC2
      "fargate" -> pure CFargate
      e ->
        fromTextError $
          "Failure parsing Compatibility from value: '" <> e
            <> "'. Accepted values: ec2, fargate"

instance ToText Compatibility where
  toText = \case
    CEC2 -> "EC2"
    CFargate -> "FARGATE"

instance Hashable Compatibility

instance NFData Compatibility

instance ToByteString Compatibility

instance ToQuery Compatibility

instance ToHeader Compatibility

instance ToJSON Compatibility where
  toJSON = toJSONText

instance FromJSON Compatibility where
  parseJSON = parseJSONText "Compatibility"
