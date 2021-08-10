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
-- Module      : Network.AWS.SageMaker.Types.ProcessingS3CompressionType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProcessingS3CompressionType
  ( ProcessingS3CompressionType
      ( ..,
        ProcessingS3CompressionType_Gzip,
        ProcessingS3CompressionType_None
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ProcessingS3CompressionType = ProcessingS3CompressionType'
  { fromProcessingS3CompressionType ::
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

pattern ProcessingS3CompressionType_Gzip :: ProcessingS3CompressionType
pattern ProcessingS3CompressionType_Gzip = ProcessingS3CompressionType' "Gzip"

pattern ProcessingS3CompressionType_None :: ProcessingS3CompressionType
pattern ProcessingS3CompressionType_None = ProcessingS3CompressionType' "None"

{-# COMPLETE
  ProcessingS3CompressionType_Gzip,
  ProcessingS3CompressionType_None,
  ProcessingS3CompressionType'
  #-}
