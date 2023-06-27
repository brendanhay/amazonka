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
-- Module      : Amazonka.QuickSight.Types.ColumnOrderingType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ColumnOrderingType
  ( ColumnOrderingType
      ( ..,
        ColumnOrderingType_GREATER_IS_BETTER,
        ColumnOrderingType_LESSER_IS_BETTER,
        ColumnOrderingType_SPECIFIED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ColumnOrderingType = ColumnOrderingType'
  { fromColumnOrderingType ::
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

pattern ColumnOrderingType_GREATER_IS_BETTER :: ColumnOrderingType
pattern ColumnOrderingType_GREATER_IS_BETTER = ColumnOrderingType' "GREATER_IS_BETTER"

pattern ColumnOrderingType_LESSER_IS_BETTER :: ColumnOrderingType
pattern ColumnOrderingType_LESSER_IS_BETTER = ColumnOrderingType' "LESSER_IS_BETTER"

pattern ColumnOrderingType_SPECIFIED :: ColumnOrderingType
pattern ColumnOrderingType_SPECIFIED = ColumnOrderingType' "SPECIFIED"

{-# COMPLETE
  ColumnOrderingType_GREATER_IS_BETTER,
  ColumnOrderingType_LESSER_IS_BETTER,
  ColumnOrderingType_SPECIFIED,
  ColumnOrderingType'
  #-}
