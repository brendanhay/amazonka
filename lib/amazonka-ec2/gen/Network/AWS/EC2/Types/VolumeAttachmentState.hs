{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VolumeAttachmentState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VolumeAttachmentState where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data VolumeAttachmentState
  = VAttached
  | VAttaching
  | VBusy
  | VDetached
  | VDetaching
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

instance FromText VolumeAttachmentState where
  parser =
    takeLowerText >>= \case
      "attached" -> pure VAttached
      "attaching" -> pure VAttaching
      "busy" -> pure VBusy
      "detached" -> pure VDetached
      "detaching" -> pure VDetaching
      e ->
        fromTextError $
          "Failure parsing VolumeAttachmentState from value: '" <> e
            <> "'. Accepted values: attached, attaching, busy, detached, detaching"

instance ToText VolumeAttachmentState where
  toText = \case
    VAttached -> "attached"
    VAttaching -> "attaching"
    VBusy -> "busy"
    VDetached -> "detached"
    VDetaching -> "detaching"

instance Hashable VolumeAttachmentState

instance NFData VolumeAttachmentState

instance ToByteString VolumeAttachmentState

instance ToQuery VolumeAttachmentState

instance ToHeader VolumeAttachmentState

instance FromXML VolumeAttachmentState where
  parseXML = parseXMLText "VolumeAttachmentState"
