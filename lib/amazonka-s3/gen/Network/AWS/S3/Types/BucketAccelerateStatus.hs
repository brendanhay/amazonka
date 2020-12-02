{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.BucketAccelerateStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.BucketAccelerateStatus where

import Network.AWS.Prelude
import Network.AWS.S3.Internal

data BucketAccelerateStatus
  = BASEnabled
  | BASSuspended
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

instance FromText BucketAccelerateStatus where
  parser =
    takeLowerText >>= \case
      "enabled" -> pure BASEnabled
      "suspended" -> pure BASSuspended
      e ->
        fromTextError $
          "Failure parsing BucketAccelerateStatus from value: '" <> e
            <> "'. Accepted values: enabled, suspended"

instance ToText BucketAccelerateStatus where
  toText = \case
    BASEnabled -> "Enabled"
    BASSuspended -> "Suspended"

instance Hashable BucketAccelerateStatus

instance NFData BucketAccelerateStatus

instance ToByteString BucketAccelerateStatus

instance ToQuery BucketAccelerateStatus

instance ToHeader BucketAccelerateStatus

instance FromXML BucketAccelerateStatus where
  parseXML = parseXMLText "BucketAccelerateStatus"

instance ToXML BucketAccelerateStatus where
  toXML = toXMLText
