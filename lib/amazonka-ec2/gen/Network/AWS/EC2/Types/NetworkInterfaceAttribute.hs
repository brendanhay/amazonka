{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NetworkInterfaceAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NetworkInterfaceAttribute where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data NetworkInterfaceAttribute
  = NIAAttachment
  | NIADescription
  | NIAGroupSet
  | NIASourceDestCheck
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

instance FromText NetworkInterfaceAttribute where
  parser =
    takeLowerText >>= \case
      "attachment" -> pure NIAAttachment
      "description" -> pure NIADescription
      "groupset" -> pure NIAGroupSet
      "sourcedestcheck" -> pure NIASourceDestCheck
      e ->
        fromTextError $
          "Failure parsing NetworkInterfaceAttribute from value: '" <> e
            <> "'. Accepted values: attachment, description, groupset, sourcedestcheck"

instance ToText NetworkInterfaceAttribute where
  toText = \case
    NIAAttachment -> "attachment"
    NIADescription -> "description"
    NIAGroupSet -> "groupSet"
    NIASourceDestCheck -> "sourceDestCheck"

instance Hashable NetworkInterfaceAttribute

instance NFData NetworkInterfaceAttribute

instance ToByteString NetworkInterfaceAttribute

instance ToQuery NetworkInterfaceAttribute

instance ToHeader NetworkInterfaceAttribute
