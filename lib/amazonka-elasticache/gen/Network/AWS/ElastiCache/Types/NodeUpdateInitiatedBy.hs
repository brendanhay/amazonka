{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.NodeUpdateInitiatedBy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.NodeUpdateInitiatedBy where

import Network.AWS.Prelude

data NodeUpdateInitiatedBy
  = Customer
  | System
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

instance FromText NodeUpdateInitiatedBy where
  parser =
    takeLowerText >>= \case
      "customer" -> pure Customer
      "system" -> pure System
      e ->
        fromTextError $
          "Failure parsing NodeUpdateInitiatedBy from value: '" <> e
            <> "'. Accepted values: customer, system"

instance ToText NodeUpdateInitiatedBy where
  toText = \case
    Customer -> "customer"
    System -> "system"

instance Hashable NodeUpdateInitiatedBy

instance NFData NodeUpdateInitiatedBy

instance ToByteString NodeUpdateInitiatedBy

instance ToQuery NodeUpdateInitiatedBy

instance ToHeader NodeUpdateInitiatedBy

instance FromXML NodeUpdateInitiatedBy where
  parseXML = parseXMLText "NodeUpdateInitiatedBy"
