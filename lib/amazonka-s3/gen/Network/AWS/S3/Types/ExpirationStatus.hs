{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ExpirationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ExpirationStatus where

import Network.AWS.Prelude
import Network.AWS.S3.Internal

data ExpirationStatus
  = ESDisabled
  | ESEnabled
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

instance FromText ExpirationStatus where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure ESDisabled
      "enabled" -> pure ESEnabled
      e ->
        fromTextError $
          "Failure parsing ExpirationStatus from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText ExpirationStatus where
  toText = \case
    ESDisabled -> "Disabled"
    ESEnabled -> "Enabled"

instance Hashable ExpirationStatus

instance NFData ExpirationStatus

instance ToByteString ExpirationStatus

instance ToQuery ExpirationStatus

instance ToHeader ExpirationStatus

instance FromXML ExpirationStatus where
  parseXML = parseXMLText "ExpirationStatus"

instance ToXML ExpirationStatus where
  toXML = toXMLText
