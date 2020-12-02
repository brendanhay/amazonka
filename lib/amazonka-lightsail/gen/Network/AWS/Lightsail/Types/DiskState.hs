{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.DiskState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.DiskState where

import Network.AWS.Prelude

data DiskState
  = DSAvailable
  | DSError'
  | DSInUse
  | DSPending
  | DSUnknown
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

instance FromText DiskState where
  parser =
    takeLowerText >>= \case
      "available" -> pure DSAvailable
      "error" -> pure DSError'
      "in-use" -> pure DSInUse
      "pending" -> pure DSPending
      "unknown" -> pure DSUnknown
      e ->
        fromTextError $
          "Failure parsing DiskState from value: '" <> e
            <> "'. Accepted values: available, error, in-use, pending, unknown"

instance ToText DiskState where
  toText = \case
    DSAvailable -> "available"
    DSError' -> "error"
    DSInUse -> "in-use"
    DSPending -> "pending"
    DSUnknown -> "unknown"

instance Hashable DiskState

instance NFData DiskState

instance ToByteString DiskState

instance ToQuery DiskState

instance ToHeader DiskState

instance FromJSON DiskState where
  parseJSON = parseJSONText "DiskState"
