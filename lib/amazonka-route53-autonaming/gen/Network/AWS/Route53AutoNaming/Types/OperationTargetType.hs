{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.OperationTargetType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.OperationTargetType where

import Network.AWS.Prelude

data OperationTargetType
  = OTTInstance
  | OTTNamespace
  | OTTService
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

instance FromText OperationTargetType where
  parser =
    takeLowerText >>= \case
      "instance" -> pure OTTInstance
      "namespace" -> pure OTTNamespace
      "service" -> pure OTTService
      e ->
        fromTextError $
          "Failure parsing OperationTargetType from value: '" <> e
            <> "'. Accepted values: instance, namespace, service"

instance ToText OperationTargetType where
  toText = \case
    OTTInstance -> "INSTANCE"
    OTTNamespace -> "NAMESPACE"
    OTTService -> "SERVICE"

instance Hashable OperationTargetType

instance NFData OperationTargetType

instance ToByteString OperationTargetType

instance ToQuery OperationTargetType

instance ToHeader OperationTargetType

instance FromJSON OperationTargetType where
  parseJSON = parseJSONText "OperationTargetType"
