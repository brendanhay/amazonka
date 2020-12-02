{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InterfacePermissionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InterfacePermissionType where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data InterfacePermissionType
  = EIPAssociate
  | InstanceAttach
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

instance FromText InterfacePermissionType where
  parser =
    takeLowerText >>= \case
      "eip-associate" -> pure EIPAssociate
      "instance-attach" -> pure InstanceAttach
      e ->
        fromTextError $
          "Failure parsing InterfacePermissionType from value: '" <> e
            <> "'. Accepted values: eip-associate, instance-attach"

instance ToText InterfacePermissionType where
  toText = \case
    EIPAssociate -> "EIP-ASSOCIATE"
    InstanceAttach -> "INSTANCE-ATTACH"

instance Hashable InterfacePermissionType

instance NFData InterfacePermissionType

instance ToByteString InterfacePermissionType

instance ToQuery InterfacePermissionType

instance ToHeader InterfacePermissionType

instance FromXML InterfacePermissionType where
  parseXML = parseXMLText "InterfacePermissionType"
