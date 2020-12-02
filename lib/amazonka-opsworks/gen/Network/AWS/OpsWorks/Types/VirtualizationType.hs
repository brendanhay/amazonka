{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.VirtualizationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.VirtualizationType where

import Network.AWS.Prelude

data VirtualizationType
  = HVM
  | Paravirtual
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

instance FromText VirtualizationType where
  parser =
    takeLowerText >>= \case
      "hvm" -> pure HVM
      "paravirtual" -> pure Paravirtual
      e ->
        fromTextError $
          "Failure parsing VirtualizationType from value: '" <> e
            <> "'. Accepted values: hvm, paravirtual"

instance ToText VirtualizationType where
  toText = \case
    HVM -> "hvm"
    Paravirtual -> "paravirtual"

instance Hashable VirtualizationType

instance NFData VirtualizationType

instance ToByteString VirtualizationType

instance ToQuery VirtualizationType

instance ToHeader VirtualizationType

instance FromJSON VirtualizationType where
  parseJSON = parseJSONText "VirtualizationType"
