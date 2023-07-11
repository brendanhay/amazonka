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
-- Module      : Amazonka.Firehose.Types.ProcessorParameterName
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.ProcessorParameterName
  ( ProcessorParameterName
      ( ..,
        ProcessorParameterName_BufferIntervalInSeconds,
        ProcessorParameterName_BufferSizeInMBs,
        ProcessorParameterName_Delimiter,
        ProcessorParameterName_JsonParsingEngine,
        ProcessorParameterName_LambdaArn,
        ProcessorParameterName_MetadataExtractionQuery,
        ProcessorParameterName_NumberOfRetries,
        ProcessorParameterName_RoleArn,
        ProcessorParameterName_SubRecordType
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ProcessorParameterName = ProcessorParameterName'
  { fromProcessorParameterName ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern ProcessorParameterName_BufferIntervalInSeconds :: ProcessorParameterName
pattern ProcessorParameterName_BufferIntervalInSeconds = ProcessorParameterName' "BufferIntervalInSeconds"

pattern ProcessorParameterName_BufferSizeInMBs :: ProcessorParameterName
pattern ProcessorParameterName_BufferSizeInMBs = ProcessorParameterName' "BufferSizeInMBs"

pattern ProcessorParameterName_Delimiter :: ProcessorParameterName
pattern ProcessorParameterName_Delimiter = ProcessorParameterName' "Delimiter"

pattern ProcessorParameterName_JsonParsingEngine :: ProcessorParameterName
pattern ProcessorParameterName_JsonParsingEngine = ProcessorParameterName' "JsonParsingEngine"

pattern ProcessorParameterName_LambdaArn :: ProcessorParameterName
pattern ProcessorParameterName_LambdaArn = ProcessorParameterName' "LambdaArn"

pattern ProcessorParameterName_MetadataExtractionQuery :: ProcessorParameterName
pattern ProcessorParameterName_MetadataExtractionQuery = ProcessorParameterName' "MetadataExtractionQuery"

pattern ProcessorParameterName_NumberOfRetries :: ProcessorParameterName
pattern ProcessorParameterName_NumberOfRetries = ProcessorParameterName' "NumberOfRetries"

pattern ProcessorParameterName_RoleArn :: ProcessorParameterName
pattern ProcessorParameterName_RoleArn = ProcessorParameterName' "RoleArn"

pattern ProcessorParameterName_SubRecordType :: ProcessorParameterName
pattern ProcessorParameterName_SubRecordType = ProcessorParameterName' "SubRecordType"

{-# COMPLETE
  ProcessorParameterName_BufferIntervalInSeconds,
  ProcessorParameterName_BufferSizeInMBs,
  ProcessorParameterName_Delimiter,
  ProcessorParameterName_JsonParsingEngine,
  ProcessorParameterName_LambdaArn,
  ProcessorParameterName_MetadataExtractionQuery,
  ProcessorParameterName_NumberOfRetries,
  ProcessorParameterName_RoleArn,
  ProcessorParameterName_SubRecordType,
  ProcessorParameterName'
  #-}
