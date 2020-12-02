{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ArchitectureType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ArchitectureType where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ArchitectureType
  = ATARM64
  | ATI386
  | ATX86_64
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

instance FromText ArchitectureType where
  parser =
    takeLowerText >>= \case
      "arm64" -> pure ATARM64
      "i386" -> pure ATI386
      "x86_64" -> pure ATX86_64
      e ->
        fromTextError $
          "Failure parsing ArchitectureType from value: '" <> e
            <> "'. Accepted values: arm64, i386, x86_64"

instance ToText ArchitectureType where
  toText = \case
    ATARM64 -> "arm64"
    ATI386 -> "i386"
    ATX86_64 -> "x86_64"

instance Hashable ArchitectureType

instance NFData ArchitectureType

instance ToByteString ArchitectureType

instance ToQuery ArchitectureType

instance ToHeader ArchitectureType

instance FromXML ArchitectureType where
  parseXML = parseXMLText "ArchitectureType"
