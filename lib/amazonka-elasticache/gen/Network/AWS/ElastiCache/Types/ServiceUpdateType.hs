{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.ServiceUpdateType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.ServiceUpdateType where

import Network.AWS.Prelude

data ServiceUpdateType = SecurityUpdate
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

instance FromText ServiceUpdateType where
  parser =
    takeLowerText >>= \case
      "security-update" -> pure SecurityUpdate
      e ->
        fromTextError $
          "Failure parsing ServiceUpdateType from value: '" <> e
            <> "'. Accepted values: security-update"

instance ToText ServiceUpdateType where
  toText = \case
    SecurityUpdate -> "security-update"

instance Hashable ServiceUpdateType

instance NFData ServiceUpdateType

instance ToByteString ServiceUpdateType

instance ToQuery ServiceUpdateType

instance ToHeader ServiceUpdateType

instance FromXML ServiceUpdateType where
  parseXML = parseXMLText "ServiceUpdateType"
