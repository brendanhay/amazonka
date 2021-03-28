{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.MetricsName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Kinesis.Types.MetricsName
  ( MetricsName
    ( MetricsName'
    , MetricsNameIncomingBytes
    , MetricsNameIncomingRecords
    , MetricsNameOutgoingBytes
    , MetricsNameOutgoingRecords
    , MetricsNameWriteProvisionedThroughputExceeded
    , MetricsNameReadProvisionedThroughputExceeded
    , MetricsNameIteratorAgeMilliseconds
    , MetricsNameAll
    , fromMetricsName
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype MetricsName = MetricsName'{fromMetricsName :: Core.Text}
                        deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                        Core.Generic)
                        deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                          Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                          Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                          Core.FromText, Core.ToByteString, Core.ToQuery,
                                          Core.ToHeader)

pattern MetricsNameIncomingBytes :: MetricsName
pattern MetricsNameIncomingBytes = MetricsName' "IncomingBytes"

pattern MetricsNameIncomingRecords :: MetricsName
pattern MetricsNameIncomingRecords = MetricsName' "IncomingRecords"

pattern MetricsNameOutgoingBytes :: MetricsName
pattern MetricsNameOutgoingBytes = MetricsName' "OutgoingBytes"

pattern MetricsNameOutgoingRecords :: MetricsName
pattern MetricsNameOutgoingRecords = MetricsName' "OutgoingRecords"

pattern MetricsNameWriteProvisionedThroughputExceeded :: MetricsName
pattern MetricsNameWriteProvisionedThroughputExceeded = MetricsName' "WriteProvisionedThroughputExceeded"

pattern MetricsNameReadProvisionedThroughputExceeded :: MetricsName
pattern MetricsNameReadProvisionedThroughputExceeded = MetricsName' "ReadProvisionedThroughputExceeded"

pattern MetricsNameIteratorAgeMilliseconds :: MetricsName
pattern MetricsNameIteratorAgeMilliseconds = MetricsName' "IteratorAgeMilliseconds"

pattern MetricsNameAll :: MetricsName
pattern MetricsNameAll = MetricsName' "ALL"

{-# COMPLETE 
  MetricsNameIncomingBytes,

  MetricsNameIncomingRecords,

  MetricsNameOutgoingBytes,

  MetricsNameOutgoingRecords,

  MetricsNameWriteProvisionedThroughputExceeded,

  MetricsNameReadProvisionedThroughputExceeded,

  MetricsNameIteratorAgeMilliseconds,

  MetricsNameAll,
  MetricsName'
  #-}
