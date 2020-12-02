{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.MetricsName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.MetricsName where

import Network.AWS.Prelude

data MetricsName
  = All
  | IncomingBytes
  | IncomingRecords
  | IteratorAgeMilliseconds
  | OutgoingBytes
  | OutgoingRecords
  | ReadProvisionedThroughputExceeded
  | WriteProvisionedThroughputExceeded
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

instance FromText MetricsName where
  parser =
    takeLowerText >>= \case
      "all" -> pure All
      "incomingbytes" -> pure IncomingBytes
      "incomingrecords" -> pure IncomingRecords
      "iteratoragemilliseconds" -> pure IteratorAgeMilliseconds
      "outgoingbytes" -> pure OutgoingBytes
      "outgoingrecords" -> pure OutgoingRecords
      "readprovisionedthroughputexceeded" -> pure ReadProvisionedThroughputExceeded
      "writeprovisionedthroughputexceeded" -> pure WriteProvisionedThroughputExceeded
      e ->
        fromTextError $
          "Failure parsing MetricsName from value: '" <> e
            <> "'. Accepted values: all, incomingbytes, incomingrecords, iteratoragemilliseconds, outgoingbytes, outgoingrecords, readprovisionedthroughputexceeded, writeprovisionedthroughputexceeded"

instance ToText MetricsName where
  toText = \case
    All -> "ALL"
    IncomingBytes -> "IncomingBytes"
    IncomingRecords -> "IncomingRecords"
    IteratorAgeMilliseconds -> "IteratorAgeMilliseconds"
    OutgoingBytes -> "OutgoingBytes"
    OutgoingRecords -> "OutgoingRecords"
    ReadProvisionedThroughputExceeded -> "ReadProvisionedThroughputExceeded"
    WriteProvisionedThroughputExceeded -> "WriteProvisionedThroughputExceeded"

instance Hashable MetricsName

instance NFData MetricsName

instance ToByteString MetricsName

instance ToQuery MetricsName

instance ToHeader MetricsName

instance ToJSON MetricsName where
  toJSON = toJSONText

instance FromJSON MetricsName where
  parseJSON = parseJSONText "MetricsName"
