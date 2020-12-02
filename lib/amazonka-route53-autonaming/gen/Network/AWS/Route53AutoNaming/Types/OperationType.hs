{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.OperationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.OperationType where

import Network.AWS.Prelude

data OperationType
  = CreateNamespace
  | DeleteNamespace
  | DeregisterInstance
  | RegisterInstance
  | UpdateService
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

instance FromText OperationType where
  parser =
    takeLowerText >>= \case
      "create_namespace" -> pure CreateNamespace
      "delete_namespace" -> pure DeleteNamespace
      "deregister_instance" -> pure DeregisterInstance
      "register_instance" -> pure RegisterInstance
      "update_service" -> pure UpdateService
      e ->
        fromTextError $
          "Failure parsing OperationType from value: '" <> e
            <> "'. Accepted values: create_namespace, delete_namespace, deregister_instance, register_instance, update_service"

instance ToText OperationType where
  toText = \case
    CreateNamespace -> "CREATE_NAMESPACE"
    DeleteNamespace -> "DELETE_NAMESPACE"
    DeregisterInstance -> "DEREGISTER_INSTANCE"
    RegisterInstance -> "REGISTER_INSTANCE"
    UpdateService -> "UPDATE_SERVICE"

instance Hashable OperationType

instance NFData OperationType

instance ToByteString OperationType

instance ToQuery OperationType

instance ToHeader OperationType

instance FromJSON OperationType where
  parseJSON = parseJSONText "OperationType"
