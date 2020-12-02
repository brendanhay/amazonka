{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.InvocationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.InvocationType where

import Network.AWS.Prelude

data InvocationType
  = Event
  | RequestResponse
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

instance FromText InvocationType where
  parser =
    takeLowerText >>= \case
      "event" -> pure Event
      "requestresponse" -> pure RequestResponse
      e ->
        fromTextError $
          "Failure parsing InvocationType from value: '" <> e
            <> "'. Accepted values: event, requestresponse"

instance ToText InvocationType where
  toText = \case
    Event -> "Event"
    RequestResponse -> "RequestResponse"

instance Hashable InvocationType

instance NFData InvocationType

instance ToByteString InvocationType

instance ToQuery InvocationType

instance ToHeader InvocationType

instance FromXML InvocationType where
  parseXML = parseXMLText "InvocationType"
