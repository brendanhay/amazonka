{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FpgaImageStateCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FpgaImageStateCode where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data FpgaImageStateCode
  = FISCAvailable
  | FISCFailed
  | FISCPending
  | FISCUnavailable
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

instance FromText FpgaImageStateCode where
  parser =
    takeLowerText >>= \case
      "available" -> pure FISCAvailable
      "failed" -> pure FISCFailed
      "pending" -> pure FISCPending
      "unavailable" -> pure FISCUnavailable
      e ->
        fromTextError $
          "Failure parsing FpgaImageStateCode from value: '" <> e
            <> "'. Accepted values: available, failed, pending, unavailable"

instance ToText FpgaImageStateCode where
  toText = \case
    FISCAvailable -> "available"
    FISCFailed -> "failed"
    FISCPending -> "pending"
    FISCUnavailable -> "unavailable"

instance Hashable FpgaImageStateCode

instance NFData FpgaImageStateCode

instance ToByteString FpgaImageStateCode

instance ToQuery FpgaImageStateCode

instance ToHeader FpgaImageStateCode

instance FromXML FpgaImageStateCode where
  parseXML = parseXMLText "FpgaImageStateCode"
