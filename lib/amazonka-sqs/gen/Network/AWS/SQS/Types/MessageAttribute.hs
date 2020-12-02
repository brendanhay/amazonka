{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.Types.MessageAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SQS.Types.MessageAttribute where

import Network.AWS.Prelude

data MessageAttribute
  = All
  | ApproximateFirstReceiveTimestamp
  | ApproximateReceiveCount
  | SenderId
  | SentTimestamp
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

instance FromText MessageAttribute where
  parser =
    takeLowerText >>= \case
      "all" -> pure All
      "approximatefirstreceivetimestamp" -> pure ApproximateFirstReceiveTimestamp
      "approximatereceivecount" -> pure ApproximateReceiveCount
      "senderid" -> pure SenderId
      "senttimestamp" -> pure SentTimestamp
      e ->
        fromTextError $
          "Failure parsing MessageAttribute from value: '" <> e
            <> "'. Accepted values: all, approximatefirstreceivetimestamp, approximatereceivecount, senderid, senttimestamp"

instance ToText MessageAttribute where
  toText = \case
    All -> "All"
    ApproximateFirstReceiveTimestamp -> "ApproximateFirstReceiveTimestamp"
    ApproximateReceiveCount -> "ApproximateReceiveCount"
    SenderId -> "SenderId"
    SentTimestamp -> "SentTimestamp"

instance Hashable MessageAttribute

instance NFData MessageAttribute

instance ToByteString MessageAttribute

instance ToQuery MessageAttribute

instance ToHeader MessageAttribute

instance FromXML MessageAttribute where
  parseXML = parseXMLText "MessageAttribute"
