{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.StatusType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.StatusType where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data StatusType
  = STFailed
  | STInitializing
  | STInsufficientData
  | STPassed
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

instance FromText StatusType where
  parser =
    takeLowerText >>= \case
      "failed" -> pure STFailed
      "initializing" -> pure STInitializing
      "insufficient-data" -> pure STInsufficientData
      "passed" -> pure STPassed
      e ->
        fromTextError $
          "Failure parsing StatusType from value: '" <> e
            <> "'. Accepted values: failed, initializing, insufficient-data, passed"

instance ToText StatusType where
  toText = \case
    STFailed -> "failed"
    STInitializing -> "initializing"
    STInsufficientData -> "insufficient-data"
    STPassed -> "passed"

instance Hashable StatusType

instance NFData StatusType

instance ToByteString StatusType

instance ToQuery StatusType

instance ToHeader StatusType

instance FromXML StatusType where
  parseXML = parseXMLText "StatusType"
