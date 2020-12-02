{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.BucketVersioningStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.BucketVersioningStatus where

import Network.AWS.Prelude
import Network.AWS.S3.Internal

data BucketVersioningStatus
  = BVSEnabled
  | BVSSuspended
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

instance FromText BucketVersioningStatus where
  parser =
    takeLowerText >>= \case
      "enabled" -> pure BVSEnabled
      "suspended" -> pure BVSSuspended
      e ->
        fromTextError $
          "Failure parsing BucketVersioningStatus from value: '" <> e
            <> "'. Accepted values: enabled, suspended"

instance ToText BucketVersioningStatus where
  toText = \case
    BVSEnabled -> "Enabled"
    BVSSuspended -> "Suspended"

instance Hashable BucketVersioningStatus

instance NFData BucketVersioningStatus

instance ToByteString BucketVersioningStatus

instance ToQuery BucketVersioningStatus

instance ToHeader BucketVersioningStatus

instance FromXML BucketVersioningStatus where
  parseXML = parseXMLText "BucketVersioningStatus"

instance ToXML BucketVersioningStatus where
  toXML = toXMLText
