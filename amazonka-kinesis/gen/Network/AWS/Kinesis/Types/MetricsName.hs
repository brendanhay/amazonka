{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.MetricsName
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.MetricsName
  ( MetricsName
      ( ..,
        MetricsName_ALL,
        MetricsName_IncomingBytes,
        MetricsName_IncomingRecords,
        MetricsName_IteratorAgeMilliseconds,
        MetricsName_OutgoingBytes,
        MetricsName_OutgoingRecords,
        MetricsName_ReadProvisionedThroughputExceeded,
        MetricsName_WriteProvisionedThroughputExceeded
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype MetricsName = MetricsName'
  { fromMetricsName ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern MetricsName_ALL :: MetricsName
pattern MetricsName_ALL = MetricsName' "ALL"

pattern MetricsName_IncomingBytes :: MetricsName
pattern MetricsName_IncomingBytes = MetricsName' "IncomingBytes"

pattern MetricsName_IncomingRecords :: MetricsName
pattern MetricsName_IncomingRecords = MetricsName' "IncomingRecords"

pattern MetricsName_IteratorAgeMilliseconds :: MetricsName
pattern MetricsName_IteratorAgeMilliseconds = MetricsName' "IteratorAgeMilliseconds"

pattern MetricsName_OutgoingBytes :: MetricsName
pattern MetricsName_OutgoingBytes = MetricsName' "OutgoingBytes"

pattern MetricsName_OutgoingRecords :: MetricsName
pattern MetricsName_OutgoingRecords = MetricsName' "OutgoingRecords"

pattern MetricsName_ReadProvisionedThroughputExceeded :: MetricsName
pattern MetricsName_ReadProvisionedThroughputExceeded = MetricsName' "ReadProvisionedThroughputExceeded"

pattern MetricsName_WriteProvisionedThroughputExceeded :: MetricsName
pattern MetricsName_WriteProvisionedThroughputExceeded = MetricsName' "WriteProvisionedThroughputExceeded"

{-# COMPLETE
  MetricsName_ALL,
  MetricsName_IncomingBytes,
  MetricsName_IncomingRecords,
  MetricsName_IteratorAgeMilliseconds,
  MetricsName_OutgoingBytes,
  MetricsName_OutgoingRecords,
  MetricsName_ReadProvisionedThroughputExceeded,
  MetricsName_WriteProvisionedThroughputExceeded,
  MetricsName'
  #-}
