{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.ImageState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.ImageState where

import Network.AWS.Prelude

data ImageState
  = ISAvailable
  | ISCopying
  | ISDeleting
  | ISFailed
  | ISPending
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

instance FromText ImageState where
  parser =
    takeLowerText >>= \case
      "available" -> pure ISAvailable
      "copying" -> pure ISCopying
      "deleting" -> pure ISDeleting
      "failed" -> pure ISFailed
      "pending" -> pure ISPending
      e ->
        fromTextError $
          "Failure parsing ImageState from value: '" <> e
            <> "'. Accepted values: available, copying, deleting, failed, pending"

instance ToText ImageState where
  toText = \case
    ISAvailable -> "AVAILABLE"
    ISCopying -> "COPYING"
    ISDeleting -> "DELETING"
    ISFailed -> "FAILED"
    ISPending -> "PENDING"

instance Hashable ImageState

instance NFData ImageState

instance ToByteString ImageState

instance ToQuery ImageState

instance ToHeader ImageState

instance FromJSON ImageState where
  parseJSON = parseJSONText "ImageState"
