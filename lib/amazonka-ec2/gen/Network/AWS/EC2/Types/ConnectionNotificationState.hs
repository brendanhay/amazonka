{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ConnectionNotificationState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ConnectionNotificationState where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ConnectionNotificationState
  = CNSDisabled
  | CNSEnabled
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

instance FromText ConnectionNotificationState where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure CNSDisabled
      "enabled" -> pure CNSEnabled
      e ->
        fromTextError $
          "Failure parsing ConnectionNotificationState from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText ConnectionNotificationState where
  toText = \case
    CNSDisabled -> "Disabled"
    CNSEnabled -> "Enabled"

instance Hashable ConnectionNotificationState

instance NFData ConnectionNotificationState

instance ToByteString ConnectionNotificationState

instance ToQuery ConnectionNotificationState

instance ToHeader ConnectionNotificationState

instance FromXML ConnectionNotificationState where
  parseXML = parseXMLText "ConnectionNotificationState"
