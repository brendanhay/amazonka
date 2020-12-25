{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.ParquetVersionValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.ParquetVersionValue
  ( ParquetVersionValue
      ( ParquetVersionValue',
        ParquetVersionValueParquet10,
        ParquetVersionValueParquet20,
        fromParquetVersionValue
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ParquetVersionValue = ParquetVersionValue'
  { fromParquetVersionValue ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern ParquetVersionValueParquet10 :: ParquetVersionValue
pattern ParquetVersionValueParquet10 = ParquetVersionValue' "parquet-1-0"

pattern ParquetVersionValueParquet20 :: ParquetVersionValue
pattern ParquetVersionValueParquet20 = ParquetVersionValue' "parquet-2-0"

{-# COMPLETE
  ParquetVersionValueParquet10,
  ParquetVersionValueParquet20,
  ParquetVersionValue'
  #-}
