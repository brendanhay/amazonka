{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.IPSetDescriptorType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.IPSetDescriptorType where

import Network.AWS.Prelude

data IPSetDescriptorType
  = IPV4
  | IPV6
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

instance FromText IPSetDescriptorType where
  parser =
    takeLowerText >>= \case
      "ipv4" -> pure IPV4
      "ipv6" -> pure IPV6
      e ->
        fromTextError $
          "Failure parsing IPSetDescriptorType from value: '" <> e
            <> "'. Accepted values: ipv4, ipv6"

instance ToText IPSetDescriptorType where
  toText = \case
    IPV4 -> "IPV4"
    IPV6 -> "IPV6"

instance Hashable IPSetDescriptorType

instance NFData IPSetDescriptorType

instance ToByteString IPSetDescriptorType

instance ToQuery IPSetDescriptorType

instance ToHeader IPSetDescriptorType

instance ToJSON IPSetDescriptorType where
  toJSON = toJSONText

instance FromJSON IPSetDescriptorType where
  parseJSON = parseJSONText "IPSetDescriptorType"
