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
-- Module      : Amazonka.SSM.Types.InventorySchemaDeleteOption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.InventorySchemaDeleteOption
  ( InventorySchemaDeleteOption
      ( ..,
        InventorySchemaDeleteOption_DeleteSchema,
        InventorySchemaDeleteOption_DisableSchema
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype InventorySchemaDeleteOption = InventorySchemaDeleteOption'
  { fromInventorySchemaDeleteOption ::
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

pattern InventorySchemaDeleteOption_DeleteSchema :: InventorySchemaDeleteOption
pattern InventorySchemaDeleteOption_DeleteSchema = InventorySchemaDeleteOption' "DeleteSchema"

pattern InventorySchemaDeleteOption_DisableSchema :: InventorySchemaDeleteOption
pattern InventorySchemaDeleteOption_DisableSchema = InventorySchemaDeleteOption' "DisableSchema"

{-# COMPLETE
  InventorySchemaDeleteOption_DeleteSchema,
  InventorySchemaDeleteOption_DisableSchema,
  InventorySchemaDeleteOption'
  #-}
