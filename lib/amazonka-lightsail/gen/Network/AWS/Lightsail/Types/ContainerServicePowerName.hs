{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ContainerServicePowerName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ContainerServicePowerName where

import Network.AWS.Prelude

data ContainerServicePowerName
  = Large
  | Medium
  | Micro
  | Nano
  | Small
  | XLarge
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

instance FromText ContainerServicePowerName where
  parser =
    takeLowerText >>= \case
      "large" -> pure Large
      "medium" -> pure Medium
      "micro" -> pure Micro
      "nano" -> pure Nano
      "small" -> pure Small
      "xlarge" -> pure XLarge
      e ->
        fromTextError $
          "Failure parsing ContainerServicePowerName from value: '" <> e
            <> "'. Accepted values: large, medium, micro, nano, small, xlarge"

instance ToText ContainerServicePowerName where
  toText = \case
    Large -> "large"
    Medium -> "medium"
    Micro -> "micro"
    Nano -> "nano"
    Small -> "small"
    XLarge -> "xlarge"

instance Hashable ContainerServicePowerName

instance NFData ContainerServicePowerName

instance ToByteString ContainerServicePowerName

instance ToQuery ContainerServicePowerName

instance ToHeader ContainerServicePowerName

instance ToJSON ContainerServicePowerName where
  toJSON = toJSONText

instance FromJSON ContainerServicePowerName where
  parseJSON = parseJSONText "ContainerServicePowerName"
