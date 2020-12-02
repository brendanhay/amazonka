{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ArchitectureValues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ArchitectureValues where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ArchitectureValues
  = ARM64
  | I386
  | X86_64
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

instance FromText ArchitectureValues where
  parser =
    takeLowerText >>= \case
      "arm64" -> pure ARM64
      "i386" -> pure I386
      "x86_64" -> pure X86_64
      e ->
        fromTextError $
          "Failure parsing ArchitectureValues from value: '" <> e
            <> "'. Accepted values: arm64, i386, x86_64"

instance ToText ArchitectureValues where
  toText = \case
    ARM64 -> "arm64"
    I386 -> "i386"
    X86_64 -> "x86_64"

instance Hashable ArchitectureValues

instance NFData ArchitectureValues

instance ToByteString ArchitectureValues

instance ToQuery ArchitectureValues

instance ToHeader ArchitectureValues

instance FromXML ArchitectureValues where
  parseXML = parseXMLText "ArchitectureValues"
