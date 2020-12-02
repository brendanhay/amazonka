{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.EFSTransitEncryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.EFSTransitEncryption where

import Network.AWS.Prelude

data EFSTransitEncryption
  = EFSTEDisabled
  | EFSTEEnabled
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

instance FromText EFSTransitEncryption where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure EFSTEDisabled
      "enabled" -> pure EFSTEEnabled
      e ->
        fromTextError $
          "Failure parsing EFSTransitEncryption from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText EFSTransitEncryption where
  toText = \case
    EFSTEDisabled -> "DISABLED"
    EFSTEEnabled -> "ENABLED"

instance Hashable EFSTransitEncryption

instance NFData EFSTransitEncryption

instance ToByteString EFSTransitEncryption

instance ToQuery EFSTransitEncryption

instance ToHeader EFSTransitEncryption

instance ToJSON EFSTransitEncryption where
  toJSON = toJSONText

instance FromJSON EFSTransitEncryption where
  parseJSON = parseJSONText "EFSTransitEncryption"
