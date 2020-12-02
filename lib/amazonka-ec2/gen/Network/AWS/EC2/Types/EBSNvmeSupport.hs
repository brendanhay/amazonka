{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.EBSNvmeSupport
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.EBSNvmeSupport where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data EBSNvmeSupport
  = ENSRequired
  | ENSSupported
  | ENSUnsupported
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

instance FromText EBSNvmeSupport where
  parser =
    takeLowerText >>= \case
      "required" -> pure ENSRequired
      "supported" -> pure ENSSupported
      "unsupported" -> pure ENSUnsupported
      e ->
        fromTextError $
          "Failure parsing EBSNvmeSupport from value: '" <> e
            <> "'. Accepted values: required, supported, unsupported"

instance ToText EBSNvmeSupport where
  toText = \case
    ENSRequired -> "required"
    ENSSupported -> "supported"
    ENSUnsupported -> "unsupported"

instance Hashable EBSNvmeSupport

instance NFData EBSNvmeSupport

instance ToByteString EBSNvmeSupport

instance ToQuery EBSNvmeSupport

instance ToHeader EBSNvmeSupport

instance FromXML EBSNvmeSupport where
  parseXML = parseXMLText "EBSNvmeSupport"
