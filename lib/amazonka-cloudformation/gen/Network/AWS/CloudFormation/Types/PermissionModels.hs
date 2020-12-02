{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.PermissionModels
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.PermissionModels where

import Network.AWS.Prelude

data PermissionModels
  = SelfManaged
  | ServiceManaged
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

instance FromText PermissionModels where
  parser =
    takeLowerText >>= \case
      "self_managed" -> pure SelfManaged
      "service_managed" -> pure ServiceManaged
      e ->
        fromTextError $
          "Failure parsing PermissionModels from value: '" <> e
            <> "'. Accepted values: self_managed, service_managed"

instance ToText PermissionModels where
  toText = \case
    SelfManaged -> "SELF_MANAGED"
    ServiceManaged -> "SERVICE_MANAGED"

instance Hashable PermissionModels

instance NFData PermissionModels

instance ToByteString PermissionModels

instance ToQuery PermissionModels

instance ToHeader PermissionModels

instance FromXML PermissionModels where
  parseXML = parseXMLText "PermissionModels"
