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
-- Module      : Amazonka.SageMaker.Types.AthenaResultCompressionType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.AthenaResultCompressionType
  ( AthenaResultCompressionType
      ( ..,
        AthenaResultCompressionType_GZIP,
        AthenaResultCompressionType_SNAPPY,
        AthenaResultCompressionType_ZLIB
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The compression used for Athena query results.
newtype AthenaResultCompressionType = AthenaResultCompressionType'
  { fromAthenaResultCompressionType ::
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

pattern AthenaResultCompressionType_GZIP :: AthenaResultCompressionType
pattern AthenaResultCompressionType_GZIP = AthenaResultCompressionType' "GZIP"

pattern AthenaResultCompressionType_SNAPPY :: AthenaResultCompressionType
pattern AthenaResultCompressionType_SNAPPY = AthenaResultCompressionType' "SNAPPY"

pattern AthenaResultCompressionType_ZLIB :: AthenaResultCompressionType
pattern AthenaResultCompressionType_ZLIB = AthenaResultCompressionType' "ZLIB"

{-# COMPLETE
  AthenaResultCompressionType_GZIP,
  AthenaResultCompressionType_SNAPPY,
  AthenaResultCompressionType_ZLIB,
  AthenaResultCompressionType'
  #-}
