{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.EnaSupport
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.EnaSupport where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data EnaSupport
  = ESRequired
  | ESSupported
  | ESUnsupported
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

instance FromText EnaSupport where
  parser =
    takeLowerText >>= \case
      "required" -> pure ESRequired
      "supported" -> pure ESSupported
      "unsupported" -> pure ESUnsupported
      e ->
        fromTextError $
          "Failure parsing EnaSupport from value: '" <> e
            <> "'. Accepted values: required, supported, unsupported"

instance ToText EnaSupport where
  toText = \case
    ESRequired -> "required"
    ESSupported -> "supported"
    ESUnsupported -> "unsupported"

instance Hashable EnaSupport

instance NFData EnaSupport

instance ToByteString EnaSupport

instance ToQuery EnaSupport

instance ToHeader EnaSupport

instance FromXML EnaSupport where
  parseXML = parseXMLText "EnaSupport"
