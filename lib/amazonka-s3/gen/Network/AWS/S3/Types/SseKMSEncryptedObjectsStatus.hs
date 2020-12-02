{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.SseKMSEncryptedObjectsStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.SseKMSEncryptedObjectsStatus where

import Network.AWS.Prelude
import Network.AWS.S3.Internal

data SseKMSEncryptedObjectsStatus
  = SKEOSDisabled
  | SKEOSEnabled
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

instance FromText SseKMSEncryptedObjectsStatus where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure SKEOSDisabled
      "enabled" -> pure SKEOSEnabled
      e ->
        fromTextError $
          "Failure parsing SseKMSEncryptedObjectsStatus from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText SseKMSEncryptedObjectsStatus where
  toText = \case
    SKEOSDisabled -> "Disabled"
    SKEOSEnabled -> "Enabled"

instance Hashable SseKMSEncryptedObjectsStatus

instance NFData SseKMSEncryptedObjectsStatus

instance ToByteString SseKMSEncryptedObjectsStatus

instance ToQuery SseKMSEncryptedObjectsStatus

instance ToHeader SseKMSEncryptedObjectsStatus

instance FromXML SseKMSEncryptedObjectsStatus where
  parseXML = parseXMLText "SseKMSEncryptedObjectsStatus"

instance ToXML SseKMSEncryptedObjectsStatus where
  toXML = toXMLText
