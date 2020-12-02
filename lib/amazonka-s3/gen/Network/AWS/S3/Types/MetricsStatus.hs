{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.MetricsStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.MetricsStatus where

import Network.AWS.Prelude
import Network.AWS.S3.Internal

data MetricsStatus
  = MSDisabled
  | MSEnabled
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

instance FromText MetricsStatus where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure MSDisabled
      "enabled" -> pure MSEnabled
      e ->
        fromTextError $
          "Failure parsing MetricsStatus from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText MetricsStatus where
  toText = \case
    MSDisabled -> "Disabled"
    MSEnabled -> "Enabled"

instance Hashable MetricsStatus

instance NFData MetricsStatus

instance ToByteString MetricsStatus

instance ToQuery MetricsStatus

instance ToHeader MetricsStatus

instance FromXML MetricsStatus where
  parseXML = parseXMLText "MetricsStatus"

instance ToXML MetricsStatus where
  toXML = toXMLText
