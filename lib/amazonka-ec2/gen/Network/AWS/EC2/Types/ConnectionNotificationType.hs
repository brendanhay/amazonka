{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ConnectionNotificationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ConnectionNotificationType where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ConnectionNotificationType = Topic
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

instance FromText ConnectionNotificationType where
  parser =
    takeLowerText >>= \case
      "topic" -> pure Topic
      e ->
        fromTextError $
          "Failure parsing ConnectionNotificationType from value: '" <> e
            <> "'. Accepted values: topic"

instance ToText ConnectionNotificationType where
  toText = \case
    Topic -> "Topic"

instance Hashable ConnectionNotificationType

instance NFData ConnectionNotificationType

instance ToByteString ConnectionNotificationType

instance ToQuery ConnectionNotificationType

instance ToHeader ConnectionNotificationType

instance FromXML ConnectionNotificationType where
  parseXML = parseXMLText "ConnectionNotificationType"
