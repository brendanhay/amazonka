{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.PlatformStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.PlatformStatus where

import Network.AWS.Prelude

data PlatformStatus
  = Creating
  | Deleted
  | Deleting
  | Failed
  | Ready
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

instance FromText PlatformStatus where
  parser =
    takeLowerText >>= \case
      "creating" -> pure Creating
      "deleted" -> pure Deleted
      "deleting" -> pure Deleting
      "failed" -> pure Failed
      "ready" -> pure Ready
      e ->
        fromTextError $
          "Failure parsing PlatformStatus from value: '" <> e
            <> "'. Accepted values: creating, deleted, deleting, failed, ready"

instance ToText PlatformStatus where
  toText = \case
    Creating -> "Creating"
    Deleted -> "Deleted"
    Deleting -> "Deleting"
    Failed -> "Failed"
    Ready -> "Ready"

instance Hashable PlatformStatus

instance NFData PlatformStatus

instance ToByteString PlatformStatus

instance ToQuery PlatformStatus

instance ToHeader PlatformStatus

instance FromXML PlatformStatus where
  parseXML = parseXMLText "PlatformStatus"
