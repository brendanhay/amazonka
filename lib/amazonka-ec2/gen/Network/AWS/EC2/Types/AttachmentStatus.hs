{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AttachmentStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AttachmentStatus where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data AttachmentStatus
  = AAttached
  | AAttaching
  | AAvailable
  | ABusy
  | ADetached
  | ADetaching
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

instance FromText AttachmentStatus where
  parser =
    takeLowerText >>= \case
      "attached" -> pure AAttached
      "attaching" -> pure AAttaching
      "available" -> pure AAvailable
      "busy" -> pure ABusy
      "detached" -> pure ADetached
      "detaching" -> pure ADetaching
      e ->
        fromTextError $
          "Failure parsing AttachmentStatus from value: '" <> e
            <> "'. Accepted values: attached, attaching, available, busy, detached, detaching"

instance ToText AttachmentStatus where
  toText = \case
    AAttached -> "attached"
    AAttaching -> "attaching"
    AAvailable -> "available"
    ABusy -> "busy"
    ADetached -> "detached"
    ADetaching -> "detaching"

instance Hashable AttachmentStatus

instance NFData AttachmentStatus

instance ToByteString AttachmentStatus

instance ToQuery AttachmentStatus

instance ToHeader AttachmentStatus

instance FromXML AttachmentStatus where
  parseXML = parseXMLText "AttachmentStatus"
