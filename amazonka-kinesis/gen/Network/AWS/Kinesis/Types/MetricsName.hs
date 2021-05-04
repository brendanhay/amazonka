{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype MetricsName = MetricsName'
  { fromMetricsName ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
