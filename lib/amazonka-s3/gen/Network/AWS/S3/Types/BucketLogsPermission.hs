{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.BucketLogsPermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.BucketLogsPermission where

import Network.AWS.Prelude
import Network.AWS.S3.Internal

data BucketLogsPermission
  = FullControl
  | Read
  | Write
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

instance FromText BucketLogsPermission where
  parser =
    takeLowerText >>= \case
      "full_control" -> pure FullControl
      "read" -> pure Read
      "write" -> pure Write
      e ->
        fromTextError $
          "Failure parsing BucketLogsPermission from value: '" <> e
            <> "'. Accepted values: full_control, read, write"

instance ToText BucketLogsPermission where
  toText = \case
    FullControl -> "FULL_CONTROL"
    Read -> "READ"
    Write -> "WRITE"

instance Hashable BucketLogsPermission

instance NFData BucketLogsPermission

instance ToByteString BucketLogsPermission

instance ToQuery BucketLogsPermission

instance ToHeader BucketLogsPermission

instance FromXML BucketLogsPermission where
  parseXML = parseXMLText "BucketLogsPermission"

instance ToXML BucketLogsPermission where
  toXML = toXMLText
