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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
import qualified Amazonka.Prelude as Prelude

-- | The compression used for Athena query results.
newtype AthenaResultCompressionType = AthenaResultCompressionType'
  { fromAthenaResultCompressionType ::
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
