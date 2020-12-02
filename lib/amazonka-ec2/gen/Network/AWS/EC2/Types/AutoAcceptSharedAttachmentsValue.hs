{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AutoAcceptSharedAttachmentsValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AutoAcceptSharedAttachmentsValue where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data AutoAcceptSharedAttachmentsValue
  = Disable
  | Enable
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

instance FromText AutoAcceptSharedAttachmentsValue where
  parser =
    takeLowerText >>= \case
      "disable" -> pure Disable
      "enable" -> pure Enable
      e ->
        fromTextError $
          "Failure parsing AutoAcceptSharedAttachmentsValue from value: '" <> e
            <> "'. Accepted values: disable, enable"

instance ToText AutoAcceptSharedAttachmentsValue where
  toText = \case
    Disable -> "disable"
    Enable -> "enable"

instance Hashable AutoAcceptSharedAttachmentsValue

instance NFData AutoAcceptSharedAttachmentsValue

instance ToByteString AutoAcceptSharedAttachmentsValue

instance ToQuery AutoAcceptSharedAttachmentsValue

instance ToHeader AutoAcceptSharedAttachmentsValue

instance FromXML AutoAcceptSharedAttachmentsValue where
  parseXML = parseXMLText "AutoAcceptSharedAttachmentsValue"
