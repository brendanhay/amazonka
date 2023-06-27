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
-- Module      : Amazonka.CostAndUsageReport.Types.SchemaElement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostAndUsageReport.Types.SchemaElement
  ( SchemaElement
      ( ..,
        SchemaElement_RESOURCES,
        SchemaElement_SPLIT_COST_ALLOCATION_DATA
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Whether or not AWS includes resource IDs in the report.
newtype SchemaElement = SchemaElement'
  { fromSchemaElement ::
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

pattern SchemaElement_RESOURCES :: SchemaElement
pattern SchemaElement_RESOURCES = SchemaElement' "RESOURCES"

pattern SchemaElement_SPLIT_COST_ALLOCATION_DATA :: SchemaElement
pattern SchemaElement_SPLIT_COST_ALLOCATION_DATA = SchemaElement' "SPLIT_COST_ALLOCATION_DATA"

{-# COMPLETE
  SchemaElement_RESOURCES,
  SchemaElement_SPLIT_COST_ALLOCATION_DATA,
  SchemaElement'
  #-}
