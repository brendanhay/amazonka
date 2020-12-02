{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VPCAttributeName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VPCAttributeName where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data VPCAttributeName
  = EnableDNSHostnames
  | EnableDNSSupport
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

instance FromText VPCAttributeName where
  parser =
    takeLowerText >>= \case
      "enablednshostnames" -> pure EnableDNSHostnames
      "enablednssupport" -> pure EnableDNSSupport
      e ->
        fromTextError $
          "Failure parsing VPCAttributeName from value: '" <> e
            <> "'. Accepted values: enablednshostnames, enablednssupport"

instance ToText VPCAttributeName where
  toText = \case
    EnableDNSHostnames -> "enableDnsHostnames"
    EnableDNSSupport -> "enableDnsSupport"

instance Hashable VPCAttributeName

instance NFData VPCAttributeName

instance ToByteString VPCAttributeName

instance ToQuery VPCAttributeName

instance ToHeader VPCAttributeName
