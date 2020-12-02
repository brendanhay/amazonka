{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.EphemeralNvmeSupport
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.EphemeralNvmeSupport where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data EphemeralNvmeSupport
  = ERequired
  | ESupported
  | EUnsupported
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

instance FromText EphemeralNvmeSupport where
  parser =
    takeLowerText >>= \case
      "required" -> pure ERequired
      "supported" -> pure ESupported
      "unsupported" -> pure EUnsupported
      e ->
        fromTextError $
          "Failure parsing EphemeralNvmeSupport from value: '" <> e
            <> "'. Accepted values: required, supported, unsupported"

instance ToText EphemeralNvmeSupport where
  toText = \case
    ERequired -> "required"
    ESupported -> "supported"
    EUnsupported -> "unsupported"

instance Hashable EphemeralNvmeSupport

instance NFData EphemeralNvmeSupport

instance ToByteString EphemeralNvmeSupport

instance ToQuery EphemeralNvmeSupport

instance ToHeader EphemeralNvmeSupport

instance FromXML EphemeralNvmeSupport where
  parseXML = parseXMLText "EphemeralNvmeSupport"
