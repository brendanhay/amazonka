-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.ProcessorParameterName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.ProcessorParameterName
  ( ProcessorParameterName
      ( ProcessorParameterName',
        BufferIntervalInSeconds,
        BufferSizeInMBs,
        LambdaARN,
        NumberOfRetries,
        RoleARN
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ProcessorParameterName = ProcessorParameterName' Lude.Text
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

pattern BufferIntervalInSeconds :: ProcessorParameterName
pattern BufferIntervalInSeconds = ProcessorParameterName' "BufferIntervalInSeconds"

pattern BufferSizeInMBs :: ProcessorParameterName
pattern BufferSizeInMBs = ProcessorParameterName' "BufferSizeInMBs"

pattern LambdaARN :: ProcessorParameterName
pattern LambdaARN = ProcessorParameterName' "LambdaArn"

pattern NumberOfRetries :: ProcessorParameterName
pattern NumberOfRetries = ProcessorParameterName' "NumberOfRetries"

pattern RoleARN :: ProcessorParameterName
pattern RoleARN = ProcessorParameterName' "RoleArn"

{-# COMPLETE
  BufferIntervalInSeconds,
  BufferSizeInMBs,
  LambdaARN,
  NumberOfRetries,
  RoleARN,
  ProcessorParameterName'
  #-}
