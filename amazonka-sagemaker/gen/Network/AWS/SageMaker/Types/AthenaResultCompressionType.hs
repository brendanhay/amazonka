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
-- Module      : Network.AWS.SageMaker.Types.AthenaResultCompressionType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AthenaResultCompressionType
  ( AthenaResultCompressionType
      ( ..,
        AthenaResultCompressionType_GZIP,
        AthenaResultCompressionType_SNAPPY,
        AthenaResultCompressionType_ZLIB
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | The compression used for Athena query results.
newtype AthenaResultCompressionType = AthenaResultCompressionType'
  { fromAthenaResultCompressionType ::
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
