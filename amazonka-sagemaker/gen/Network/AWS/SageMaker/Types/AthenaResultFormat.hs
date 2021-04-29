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

import qualified Network.AWS.Prelude as Prelude

-- | The data storage format for Athena query results.
newtype AthenaResultFormat = AthenaResultFormat'
  { fromAthenaResultFormat ::
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
