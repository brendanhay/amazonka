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
-- Module      : Amazonka.Athena.Types.ColumnNullable
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types.ColumnNullable
  ( ColumnNullable
      ( ..,
        ColumnNullable_NOT_NULL,
        ColumnNullable_NULLABLE,
        ColumnNullable_UNKNOWN
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ColumnNullable = ColumnNullable'
  { fromColumnNullable ::
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
