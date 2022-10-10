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
-- Module      : Amazonka.Glue.Types.PiiType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.PiiType
  ( PiiType
      ( ..,
        PiiType_ColumnAudit,
        PiiType_ColumnMasking,
        PiiType_RowAudit,
        PiiType_RowMasking
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype PiiType = PiiType' {fromPiiType :: Core.Text}
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

pattern PiiType_ColumnAudit :: PiiType
pattern PiiType_ColumnAudit = PiiType' "ColumnAudit"

pattern PiiType_ColumnMasking :: PiiType
pattern PiiType_ColumnMasking = PiiType' "ColumnMasking"

pattern PiiType_RowAudit :: PiiType
pattern PiiType_RowAudit = PiiType' "RowAudit"

pattern PiiType_RowMasking :: PiiType
pattern PiiType_RowMasking = PiiType' "RowMasking"

{-# COMPLETE
  PiiType_ColumnAudit,
  PiiType_ColumnMasking,
  PiiType_RowAudit,
  PiiType_RowMasking,
  PiiType'
  #-}
