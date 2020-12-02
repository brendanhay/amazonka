{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.HasLogicalRedundancy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.HasLogicalRedundancy where

import Network.AWS.Prelude

data HasLogicalRedundancy
  = HLRNO
  | HLRUnknown
  | HLRYes
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

instance FromText HasLogicalRedundancy where
  parser =
    takeLowerText >>= \case
      "no" -> pure HLRNO
      "unknown" -> pure HLRUnknown
      "yes" -> pure HLRYes
      e ->
        fromTextError $
          "Failure parsing HasLogicalRedundancy from value: '" <> e
            <> "'. Accepted values: no, unknown, yes"

instance ToText HasLogicalRedundancy where
  toText = \case
    HLRNO -> "no"
    HLRUnknown -> "unknown"
    HLRYes -> "yes"

instance Hashable HasLogicalRedundancy

instance NFData HasLogicalRedundancy

instance ToByteString HasLogicalRedundancy

instance ToQuery HasLogicalRedundancy

instance ToHeader HasLogicalRedundancy

instance FromJSON HasLogicalRedundancy where
  parseJSON = parseJSONText "HasLogicalRedundancy"
