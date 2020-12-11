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
        Parquet10,
        Parquet20
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ParquetVersionValue = ParquetVersionValue' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern Parquet10 :: ParquetVersionValue
pattern Parquet10 = ParquetVersionValue' "parquet-1-0"

pattern Parquet20 :: ParquetVersionValue
pattern Parquet20 = ParquetVersionValue' "parquet-2-0"

{-# COMPLETE
  Parquet10,
  Parquet20,
  ParquetVersionValue'
  #-}
