{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ImageState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ImageState where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ImageState
  = ISAvailable
  | ISDeregistered
  | ISError'
  | ISFailed
  | ISInvalid
  | ISPending
  | ISTransient
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
      "deregistered" -> pure ISDeregistered
      "error" -> pure ISError'
      "failed" -> pure ISFailed
      "invalid" -> pure ISInvalid
      "pending" -> pure ISPending
      "transient" -> pure ISTransient
      e ->
        fromTextError $
          "Failure parsing ImageState from value: '" <> e
            <> "'. Accepted values: available, deregistered, error, failed, invalid, pending, transient"

instance ToText ImageState where
  toText = \case
    ISAvailable -> "available"
    ISDeregistered -> "deregistered"
    ISError' -> "error"
    ISFailed -> "failed"
    ISInvalid -> "invalid"
    ISPending -> "pending"
    ISTransient -> "transient"

instance Hashable ImageState

instance NFData ImageState

instance ToByteString ImageState

instance ToQuery ImageState

instance ToHeader ImageState

instance FromXML ImageState where
  parseXML = parseXMLText "ImageState"
