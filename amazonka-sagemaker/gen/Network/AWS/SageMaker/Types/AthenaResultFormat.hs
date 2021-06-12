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
-- Module      : Network.AWS.SageMaker.Types.AthenaResultFormat
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AthenaResultFormat
  ( AthenaResultFormat
      ( ..,
        AthenaResultFormat_AVRO,
        AthenaResultFormat_JSON,
        AthenaResultFormat_ORC,
        AthenaResultFormat_PARQUET,
        AthenaResultFormat_TEXTFILE
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | The data storage format for Athena query results.
newtype AthenaResultFormat = AthenaResultFormat'
  { fromAthenaResultFormat ::
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

pattern AthenaResultFormat_AVRO :: AthenaResultFormat
pattern AthenaResultFormat_AVRO = AthenaResultFormat' "AVRO"

pattern AthenaResultFormat_JSON :: AthenaResultFormat
pattern AthenaResultFormat_JSON = AthenaResultFormat' "JSON"

pattern AthenaResultFormat_ORC :: AthenaResultFormat
pattern AthenaResultFormat_ORC = AthenaResultFormat' "ORC"

pattern AthenaResultFormat_PARQUET :: AthenaResultFormat
pattern AthenaResultFormat_PARQUET = AthenaResultFormat' "PARQUET"

pattern AthenaResultFormat_TEXTFILE :: AthenaResultFormat
pattern AthenaResultFormat_TEXTFILE = AthenaResultFormat' "TEXTFILE"

{-# COMPLETE
  AthenaResultFormat_AVRO,
  AthenaResultFormat_JSON,
  AthenaResultFormat_ORC,
  AthenaResultFormat_PARQUET,
  AthenaResultFormat_TEXTFILE,
  AthenaResultFormat'
  #-}
