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

import qualified Network.AWS.Prelude as Prelude

newtype ProcessingS3CompressionType = ProcessingS3CompressionType'
  { fromProcessingS3CompressionType ::
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

pattern ProcessingS3CompressionType_Gzip :: ProcessingS3CompressionType
pattern ProcessingS3CompressionType_Gzip = ProcessingS3CompressionType' "Gzip"

pattern ProcessingS3CompressionType_None :: ProcessingS3CompressionType
pattern ProcessingS3CompressionType_None = ProcessingS3CompressionType' "None"

{-# COMPLETE
  ProcessingS3CompressionType_Gzip,
  ProcessingS3CompressionType_None,
  ProcessingS3CompressionType'
  #-}
