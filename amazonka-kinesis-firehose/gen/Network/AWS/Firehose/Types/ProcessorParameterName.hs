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
-- Module      : Network.AWS.Firehose.Types.ProcessorParameterName
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.ProcessorParameterName
  ( ProcessorParameterName
      ( ..,
        ProcessorParameterName_BufferIntervalInSeconds,
        ProcessorParameterName_BufferSizeInMBs,
        ProcessorParameterName_LambdaArn,
        ProcessorParameterName_NumberOfRetries,
        ProcessorParameterName_RoleArn
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ProcessorParameterName = ProcessorParameterName'
  { fromProcessorParameterName ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
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

pattern ProcessorParameterName_BufferIntervalInSeconds :: ProcessorParameterName
pattern ProcessorParameterName_BufferIntervalInSeconds = ProcessorParameterName' "BufferIntervalInSeconds"

pattern ProcessorParameterName_BufferSizeInMBs :: ProcessorParameterName
pattern ProcessorParameterName_BufferSizeInMBs = ProcessorParameterName' "BufferSizeInMBs"

pattern ProcessorParameterName_LambdaArn :: ProcessorParameterName
pattern ProcessorParameterName_LambdaArn = ProcessorParameterName' "LambdaArn"

pattern ProcessorParameterName_NumberOfRetries :: ProcessorParameterName
pattern ProcessorParameterName_NumberOfRetries = ProcessorParameterName' "NumberOfRetries"

pattern ProcessorParameterName_RoleArn :: ProcessorParameterName
pattern ProcessorParameterName_RoleArn = ProcessorParameterName' "RoleArn"

{-# COMPLETE
  ProcessorParameterName_BufferIntervalInSeconds,
  ProcessorParameterName_BufferSizeInMBs,
  ProcessorParameterName_LambdaArn,
  ProcessorParameterName_NumberOfRetries,
  ProcessorParameterName_RoleArn,
  ProcessorParameterName'
  #-}
