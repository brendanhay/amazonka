{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.FirelensConfigurationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.FirelensConfigurationType where

import Network.AWS.Prelude

data FirelensConfigurationType
  = Fluentbit
  | Fluentd
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

instance FromText FirelensConfigurationType where
  parser =
    takeLowerText >>= \case
      "fluentbit" -> pure Fluentbit
      "fluentd" -> pure Fluentd
      e ->
        fromTextError $
          "Failure parsing FirelensConfigurationType from value: '" <> e
            <> "'. Accepted values: fluentbit, fluentd"

instance ToText FirelensConfigurationType where
  toText = \case
    Fluentbit -> "fluentbit"
    Fluentd -> "fluentd"

instance Hashable FirelensConfigurationType

instance NFData FirelensConfigurationType

instance ToByteString FirelensConfigurationType

instance ToQuery FirelensConfigurationType

instance ToHeader FirelensConfigurationType

instance ToJSON FirelensConfigurationType where
  toJSON = toJSONText

instance FromJSON FirelensConfigurationType where
  parseJSON = parseJSONText "FirelensConfigurationType"
