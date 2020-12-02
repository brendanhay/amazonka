{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.MFADeleteStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.MFADeleteStatus where

import Network.AWS.Prelude
import Network.AWS.S3.Internal

data MFADeleteStatus
  = MDSDisabled
  | MDSEnabled
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

instance FromText MFADeleteStatus where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure MDSDisabled
      "enabled" -> pure MDSEnabled
      e ->
        fromTextError $
          "Failure parsing MFADeleteStatus from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText MFADeleteStatus where
  toText = \case
    MDSDisabled -> "Disabled"
    MDSEnabled -> "Enabled"

instance Hashable MFADeleteStatus

instance NFData MFADeleteStatus

instance ToByteString MFADeleteStatus

instance ToQuery MFADeleteStatus

instance ToHeader MFADeleteStatus

instance FromXML MFADeleteStatus where
  parseXML = parseXMLText "MFADeleteStatus"
