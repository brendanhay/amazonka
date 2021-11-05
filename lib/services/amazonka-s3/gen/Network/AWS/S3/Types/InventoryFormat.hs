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
-- Module      : Amazonka.S3.Types.InventoryFormat
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.InventoryFormat
  ( InventoryFormat
      ( ..,
        InventoryFormat_CSV,
        InventoryFormat_ORC,
        InventoryFormat_Parquet
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal

newtype InventoryFormat = InventoryFormat'
  { fromInventoryFormat ::
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

pattern InventoryFormat_CSV :: InventoryFormat
pattern InventoryFormat_CSV = InventoryFormat' "CSV"

pattern InventoryFormat_ORC :: InventoryFormat
pattern InventoryFormat_ORC = InventoryFormat' "ORC"

pattern InventoryFormat_Parquet :: InventoryFormat
pattern InventoryFormat_Parquet = InventoryFormat' "Parquet"

{-# COMPLETE
  InventoryFormat_CSV,
  InventoryFormat_ORC,
  InventoryFormat_Parquet,
  InventoryFormat'
  #-}
