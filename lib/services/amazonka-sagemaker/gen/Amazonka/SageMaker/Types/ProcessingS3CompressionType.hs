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
-- Module      : Amazonka.SageMaker.Types.ProcessingS3CompressionType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ProcessingS3CompressionType
  ( ProcessingS3CompressionType
      ( ..,
        ProcessingS3CompressionType_Gzip,
        ProcessingS3CompressionType_None
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ProcessingS3CompressionType = ProcessingS3CompressionType'
  { fromProcessingS3CompressionType ::
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

pattern ProcessingS3CompressionType_Gzip :: ProcessingS3CompressionType
pattern ProcessingS3CompressionType_Gzip = ProcessingS3CompressionType' "Gzip"

pattern ProcessingS3CompressionType_None :: ProcessingS3CompressionType
pattern ProcessingS3CompressionType_None = ProcessingS3CompressionType' "None"

{-# COMPLETE
  ProcessingS3CompressionType_Gzip,
  ProcessingS3CompressionType_None,
  ProcessingS3CompressionType'
  #-}
