{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.IntelligentTieringStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.IntelligentTieringStatus where

import Network.AWS.Prelude
import Network.AWS.S3.Internal

data IntelligentTieringStatus
  = ITSDisabled
  | ITSEnabled
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

instance FromText IntelligentTieringStatus where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure ITSDisabled
      "enabled" -> pure ITSEnabled
      e ->
        fromTextError $
          "Failure parsing IntelligentTieringStatus from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText IntelligentTieringStatus where
  toText = \case
    ITSDisabled -> "Disabled"
    ITSEnabled -> "Enabled"

instance Hashable IntelligentTieringStatus

instance NFData IntelligentTieringStatus

instance ToByteString IntelligentTieringStatus

instance ToQuery IntelligentTieringStatus

instance ToHeader IntelligentTieringStatus

instance FromXML IntelligentTieringStatus where
  parseXML = parseXMLText "IntelligentTieringStatus"

instance ToXML IntelligentTieringStatus where
  toXML = toXMLText
