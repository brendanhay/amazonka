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
-- Module      : Amazonka.SageMaker.Types.RedshiftResultCompressionType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.RedshiftResultCompressionType
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The compression used for Redshift query results.
newtype RedshiftResultCompressionType = RedshiftResultCompressionType'
  { fromRedshiftResultCompressionType ::
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
