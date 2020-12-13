{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDBStreams.Types.ShardIteratorType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDBStreams.Types.ShardIteratorType
  ( ShardIteratorType
      ( ShardIteratorType',
        TrimHorizon,
        Latest,
        AtSequenceNumber,
        AfterSequenceNumber
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

pattern TrimHorizon :: ShardIteratorType
pattern TrimHorizon = ShardIteratorType' "TRIM_HORIZON"

pattern Latest :: ShardIteratorType
pattern Latest = ShardIteratorType' "LATEST"

pattern AtSequenceNumber :: ShardIteratorType
pattern AtSequenceNumber = ShardIteratorType' "AT_SEQUENCE_NUMBER"

pattern AfterSequenceNumber :: ShardIteratorType
pattern AfterSequenceNumber = ShardIteratorType' "AFTER_SEQUENCE_NUMBER"

{-# COMPLETE
  TrimHorizon,
  Latest,
  AtSequenceNumber,
  AfterSequenceNumber,
  ShardIteratorType'
  #-}
