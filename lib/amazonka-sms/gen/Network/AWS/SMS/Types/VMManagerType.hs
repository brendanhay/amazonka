{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.VMManagerType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.VMManagerType where

import Network.AWS.Prelude

data VMManagerType
  = HypervManager
  | Scvmm
  | Vsphere
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

instance FromText VMManagerType where
  parser =
    takeLowerText >>= \case
      "hyperv-manager" -> pure HypervManager
      "scvmm" -> pure Scvmm
      "vsphere" -> pure Vsphere
      e ->
        fromTextError $
          "Failure parsing VMManagerType from value: '" <> e
            <> "'. Accepted values: hyperv-manager, scvmm, vsphere"

instance ToText VMManagerType where
  toText = \case
    HypervManager -> "HYPERV-MANAGER"
    Scvmm -> "SCVMM"
    Vsphere -> "VSPHERE"

instance Hashable VMManagerType

instance NFData VMManagerType

instance ToByteString VMManagerType

instance ToQuery VMManagerType

instance ToHeader VMManagerType

instance ToJSON VMManagerType where
  toJSON = toJSONText

instance FromJSON VMManagerType where
  parseJSON = parseJSONText "VMManagerType"
