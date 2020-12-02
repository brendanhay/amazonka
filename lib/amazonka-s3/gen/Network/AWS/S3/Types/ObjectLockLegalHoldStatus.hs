{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ObjectLockLegalHoldStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ObjectLockLegalHoldStatus where

import Network.AWS.Prelude
import Network.AWS.S3.Internal

data ObjectLockLegalHoldStatus
  = ON
  | Off
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

instance FromText ObjectLockLegalHoldStatus where
  parser =
    takeLowerText >>= \case
      "on" -> pure ON
      "off" -> pure Off
      e ->
        fromTextError $
          "Failure parsing ObjectLockLegalHoldStatus from value: '" <> e
            <> "'. Accepted values: on, off"

instance ToText ObjectLockLegalHoldStatus where
  toText = \case
    ON -> "ON"
    Off -> "OFF"

instance Hashable ObjectLockLegalHoldStatus

instance NFData ObjectLockLegalHoldStatus

instance ToByteString ObjectLockLegalHoldStatus

instance ToQuery ObjectLockLegalHoldStatus

instance ToHeader ObjectLockLegalHoldStatus

instance FromXML ObjectLockLegalHoldStatus where
  parseXML = parseXMLText "ObjectLockLegalHoldStatus"

instance ToXML ObjectLockLegalHoldStatus where
  toXML = toXMLText
