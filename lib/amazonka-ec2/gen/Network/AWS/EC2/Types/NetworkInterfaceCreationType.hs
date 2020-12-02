{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NetworkInterfaceCreationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NetworkInterfaceCreationType where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data NetworkInterfaceCreationType = Efa
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

instance FromText NetworkInterfaceCreationType where
  parser =
    takeLowerText >>= \case
      "efa" -> pure Efa
      e ->
        fromTextError $
          "Failure parsing NetworkInterfaceCreationType from value: '" <> e
            <> "'. Accepted values: efa"

instance ToText NetworkInterfaceCreationType where
  toText = \case
    Efa -> "efa"

instance Hashable NetworkInterfaceCreationType

instance NFData NetworkInterfaceCreationType

instance ToByteString NetworkInterfaceCreationType

instance ToQuery NetworkInterfaceCreationType

instance ToHeader NetworkInterfaceCreationType
