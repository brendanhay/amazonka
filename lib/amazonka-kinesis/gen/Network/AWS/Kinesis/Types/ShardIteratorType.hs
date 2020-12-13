{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.ShardIteratorType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.ShardIteratorType
  ( ShardIteratorType
      ( ShardIteratorType',
        SITAtSequenceNumber,
        SITAfterSequenceNumber,
        SITTrimHorizon,
        SITLatest,
        SITAtTimestamp
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ShardIteratorType = ShardIteratorType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern SITAtSequenceNumber :: ShardIteratorType
pattern SITAtSequenceNumber = ShardIteratorType' "AT_SEQUENCE_NUMBER"

pattern SITAfterSequenceNumber :: ShardIteratorType
pattern SITAfterSequenceNumber = ShardIteratorType' "AFTER_SEQUENCE_NUMBER"

pattern SITTrimHorizon :: ShardIteratorType
pattern SITTrimHorizon = ShardIteratorType' "TRIM_HORIZON"

pattern SITLatest :: ShardIteratorType
pattern SITLatest = ShardIteratorType' "LATEST"

pattern SITAtTimestamp :: ShardIteratorType
pattern SITAtTimestamp = ShardIteratorType' "AT_TIMESTAMP"

{-# COMPLETE
  SITAtSequenceNumber,
  SITAfterSequenceNumber,
  SITTrimHorizon,
  SITLatest,
  SITAtTimestamp,
  ShardIteratorType'
  #-}
