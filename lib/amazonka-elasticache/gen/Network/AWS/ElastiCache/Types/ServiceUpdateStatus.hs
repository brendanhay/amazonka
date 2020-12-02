{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.ServiceUpdateStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.ServiceUpdateStatus where

import Network.AWS.Prelude

data ServiceUpdateStatus
  = Available
  | Cancelled
  | Expired
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

instance FromText ServiceUpdateStatus where
  parser =
    takeLowerText >>= \case
      "available" -> pure Available
      "cancelled" -> pure Cancelled
      "expired" -> pure Expired
      e ->
        fromTextError $
          "Failure parsing ServiceUpdateStatus from value: '" <> e
            <> "'. Accepted values: available, cancelled, expired"

instance ToText ServiceUpdateStatus where
  toText = \case
    Available -> "available"
    Cancelled -> "cancelled"
    Expired -> "expired"

instance Hashable ServiceUpdateStatus

instance NFData ServiceUpdateStatus

instance ToByteString ServiceUpdateStatus

instance ToQuery ServiceUpdateStatus

instance ToHeader ServiceUpdateStatus

instance FromXML ServiceUpdateStatus where
  parseXML = parseXMLText "ServiceUpdateStatus"
