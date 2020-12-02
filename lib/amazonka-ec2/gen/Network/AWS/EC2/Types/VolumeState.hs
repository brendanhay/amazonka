{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VolumeState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VolumeState where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data VolumeState
  = VAvailable
  | VCreating
  | VDeleted
  | VDeleting
  | VError'
  | VInUse
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

instance FromText VolumeState where
  parser =
    takeLowerText >>= \case
      "available" -> pure VAvailable
      "creating" -> pure VCreating
      "deleted" -> pure VDeleted
      "deleting" -> pure VDeleting
      "error" -> pure VError'
      "in-use" -> pure VInUse
      e ->
        fromTextError $
          "Failure parsing VolumeState from value: '" <> e
            <> "'. Accepted values: available, creating, deleted, deleting, error, in-use"

instance ToText VolumeState where
  toText = \case
    VAvailable -> "available"
    VCreating -> "creating"
    VDeleted -> "deleted"
    VDeleting -> "deleting"
    VError' -> "error"
    VInUse -> "in-use"

instance Hashable VolumeState

instance NFData VolumeState

instance ToByteString VolumeState

instance ToQuery VolumeState

instance ToHeader VolumeState

instance FromXML VolumeState where
  parseXML = parseXMLText "VolumeState"
