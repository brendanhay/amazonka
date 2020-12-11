-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.ColumnNullable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.ColumnNullable
  ( ColumnNullable
      ( ColumnNullable',
        NotNull,
        Nullable,
        Unknown
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ColumnNullable = ColumnNullable' Lude.Text
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

pattern NotNull :: ColumnNullable
pattern NotNull = ColumnNullable' "NOT_NULL"

pattern Nullable :: ColumnNullable
pattern Nullable = ColumnNullable' "NULLABLE"

pattern Unknown :: ColumnNullable
pattern Unknown = ColumnNullable' "UNKNOWN"

{-# COMPLETE
  NotNull,
  Nullable,
  Unknown,
  ColumnNullable'
  #-}
