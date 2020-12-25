{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        ProcessorParameterNameLambdaArn,
        ProcessorParameterNameNumberOfRetries,
        ProcessorParameterNameRoleArn,
        ProcessorParameterNameBufferSizeInMBs,
        ProcessorParameterNameBufferIntervalInSeconds,
        fromProcessorParameterName
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ProcessorParameterName = ProcessorParameterName'
  { fromProcessorParameterName ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern ProcessorParameterNameLambdaArn :: ProcessorParameterName
pattern ProcessorParameterNameLambdaArn = ProcessorParameterName' "LambdaArn"

pattern ProcessorParameterNameNumberOfRetries :: ProcessorParameterName
pattern ProcessorParameterNameNumberOfRetries = ProcessorParameterName' "NumberOfRetries"

pattern ProcessorParameterNameRoleArn :: ProcessorParameterName
pattern ProcessorParameterNameRoleArn = ProcessorParameterName' "RoleArn"

pattern ProcessorParameterNameBufferSizeInMBs :: ProcessorParameterName
pattern ProcessorParameterNameBufferSizeInMBs = ProcessorParameterName' "BufferSizeInMBs"

pattern ProcessorParameterNameBufferIntervalInSeconds :: ProcessorParameterName
pattern ProcessorParameterNameBufferIntervalInSeconds = ProcessorParameterName' "BufferIntervalInSeconds"

{-# COMPLETE
  ProcessorParameterNameLambdaArn,
  ProcessorParameterNameNumberOfRetries,
  ProcessorParameterNameRoleArn,
  ProcessorParameterNameBufferSizeInMBs,
  ProcessorParameterNameBufferIntervalInSeconds,
  ProcessorParameterName'
  #-}
