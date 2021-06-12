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
-- Module      : Network.AWS.SageMaker.Types.RedshiftResultCompressionType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.RedshiftResultCompressionType
  ( RedshiftResultCompressionType
      ( ..,
        RedshiftResultCompressionType_BZIP2,
        RedshiftResultCompressionType_GZIP,
        RedshiftResultCompressionType_None,
        RedshiftResultCompressionType_SNAPPY,
        RedshiftResultCompressionType_ZSTD
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | The compression used for Redshift query results.
newtype RedshiftResultCompressionType = RedshiftResultCompressionType'
  { fromRedshiftResultCompressionType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern RedshiftResultCompressionType_BZIP2 :: RedshiftResultCompressionType
pattern RedshiftResultCompressionType_BZIP2 = RedshiftResultCompressionType' "BZIP2"

pattern RedshiftResultCompressionType_GZIP :: RedshiftResultCompressionType
pattern RedshiftResultCompressionType_GZIP = RedshiftResultCompressionType' "GZIP"

pattern RedshiftResultCompressionType_None :: RedshiftResultCompressionType
pattern RedshiftResultCompressionType_None = RedshiftResultCompressionType' "None"

pattern RedshiftResultCompressionType_SNAPPY :: RedshiftResultCompressionType
pattern RedshiftResultCompressionType_SNAPPY = RedshiftResultCompressionType' "SNAPPY"

pattern RedshiftResultCompressionType_ZSTD :: RedshiftResultCompressionType
pattern RedshiftResultCompressionType_ZSTD = RedshiftResultCompressionType' "ZSTD"

{-# COMPLETE
  RedshiftResultCompressionType_BZIP2,
  RedshiftResultCompressionType_GZIP,
  RedshiftResultCompressionType_None,
  RedshiftResultCompressionType_SNAPPY,
  RedshiftResultCompressionType_ZSTD,
  RedshiftResultCompressionType'
  #-}
