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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ColumnNullable = ColumnNullable'
  { fromColumnNullable ::
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
