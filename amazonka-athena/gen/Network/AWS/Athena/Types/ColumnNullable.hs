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
-- Module      : Network.AWS.Athena.Types.ColumnNullable
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.ColumnNullable
  ( ColumnNullable
      ( ..,
        ColumnNullable_NOT_NULL,
        ColumnNullable_NULLABLE,
        ColumnNullable_UNKNOWN
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ColumnNullable = ColumnNullable'
  { fromColumnNullable ::
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

pattern ColumnNullable_NOT_NULL :: ColumnNullable
pattern ColumnNullable_NOT_NULL = ColumnNullable' "NOT_NULL"

pattern ColumnNullable_NULLABLE :: ColumnNullable
pattern ColumnNullable_NULLABLE = ColumnNullable' "NULLABLE"

pattern ColumnNullable_UNKNOWN :: ColumnNullable
pattern ColumnNullable_UNKNOWN = ColumnNullable' "UNKNOWN"

{-# COMPLETE
  ColumnNullable_NOT_NULL,
  ColumnNullable_NULLABLE,
  ColumnNullable_UNKNOWN,
  ColumnNullable'
  #-}
