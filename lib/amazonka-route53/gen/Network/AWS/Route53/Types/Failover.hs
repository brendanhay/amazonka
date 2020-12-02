{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.Failover
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.Failover where

import Network.AWS.Prelude
import Network.AWS.Route53.Internal

data Failover
  = Primary
  | Secondary
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

instance FromText Failover where
  parser =
    takeLowerText >>= \case
      "primary" -> pure Primary
      "secondary" -> pure Secondary
      e ->
        fromTextError $
          "Failure parsing Failover from value: '" <> e
            <> "'. Accepted values: primary, secondary"

instance ToText Failover where
  toText = \case
    Primary -> "PRIMARY"
    Secondary -> "SECONDARY"

instance Hashable Failover

instance NFData Failover

instance ToByteString Failover

instance ToQuery Failover

instance ToHeader Failover

instance FromXML Failover where
  parseXML = parseXMLText "Failover"

instance ToXML Failover where
  toXML = toXMLText
