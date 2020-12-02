{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VolumeStatusInfoStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VolumeStatusInfoStatus where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data VolumeStatusInfoStatus
  = Impaired
  | InsufficientData
  | OK
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

instance FromText VolumeStatusInfoStatus where
  parser =
    takeLowerText >>= \case
      "impaired" -> pure Impaired
      "insufficient-data" -> pure InsufficientData
      "ok" -> pure OK
      e ->
        fromTextError $
          "Failure parsing VolumeStatusInfoStatus from value: '" <> e
            <> "'. Accepted values: impaired, insufficient-data, ok"

instance ToText VolumeStatusInfoStatus where
  toText = \case
    Impaired -> "impaired"
    InsufficientData -> "insufficient-data"
    OK -> "ok"

instance Hashable VolumeStatusInfoStatus

instance NFData VolumeStatusInfoStatus

instance ToByteString VolumeStatusInfoStatus

instance ToQuery VolumeStatusInfoStatus

instance ToHeader VolumeStatusInfoStatus

instance FromXML VolumeStatusInfoStatus where
  parseXML = parseXMLText "VolumeStatusInfoStatus"
