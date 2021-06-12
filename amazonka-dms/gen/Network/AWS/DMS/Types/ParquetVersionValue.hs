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
-- Module      : Network.AWS.DMS.Types.ParquetVersionValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.ParquetVersionValue
  ( ParquetVersionValue
      ( ..,
        ParquetVersionValue_Parquet_1_0,
        ParquetVersionValue_Parquet_2_0
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ParquetVersionValue = ParquetVersionValue'
  { fromParquetVersionValue ::
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

pattern ParquetVersionValue_Parquet_1_0 :: ParquetVersionValue
pattern ParquetVersionValue_Parquet_1_0 = ParquetVersionValue' "parquet-1-0"

pattern ParquetVersionValue_Parquet_2_0 :: ParquetVersionValue
pattern ParquetVersionValue_Parquet_2_0 = ParquetVersionValue' "parquet-2-0"

{-# COMPLETE
  ParquetVersionValue_Parquet_1_0,
  ParquetVersionValue_Parquet_2_0,
  ParquetVersionValue'
  #-}
