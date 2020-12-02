{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AssociatedNetworkType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AssociatedNetworkType where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data AssociatedNetworkType = ANTVPC
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

instance FromText AssociatedNetworkType where
  parser =
    takeLowerText >>= \case
      "vpc" -> pure ANTVPC
      e ->
        fromTextError $
          "Failure parsing AssociatedNetworkType from value: '" <> e
            <> "'. Accepted values: vpc"

instance ToText AssociatedNetworkType where
  toText = \case
    ANTVPC -> "vpc"

instance Hashable AssociatedNetworkType

instance NFData AssociatedNetworkType

instance ToByteString AssociatedNetworkType

instance ToQuery AssociatedNetworkType

instance ToHeader AssociatedNetworkType

instance FromXML AssociatedNetworkType where
  parseXML = parseXMLText "AssociatedNetworkType"
