{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.BrokerStorageType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.BrokerStorageType where

import Network.AWS.Prelude

-- | /Important:/ EFS is currently not Supported for RabbitMQ engine type.
data BrokerStorageType
  = EBS
  | Efs
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

instance FromText BrokerStorageType where
  parser =
    takeLowerText >>= \case
      "ebs" -> pure EBS
      "efs" -> pure Efs
      e ->
        fromTextError $
          "Failure parsing BrokerStorageType from value: '" <> e
            <> "'. Accepted values: ebs, efs"

instance ToText BrokerStorageType where
  toText = \case
    EBS -> "EBS"
    Efs -> "EFS"

instance Hashable BrokerStorageType

instance NFData BrokerStorageType

instance ToByteString BrokerStorageType

instance ToQuery BrokerStorageType

instance ToHeader BrokerStorageType

instance ToJSON BrokerStorageType where
  toJSON = toJSONText

instance FromJSON BrokerStorageType where
  parseJSON = parseJSONText "BrokerStorageType"
