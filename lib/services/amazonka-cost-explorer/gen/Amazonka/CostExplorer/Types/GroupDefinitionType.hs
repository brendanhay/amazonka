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
-- Module      : Amazonka.CostExplorer.Types.GroupDefinitionType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.GroupDefinitionType
  ( GroupDefinitionType
      ( ..,
        GroupDefinitionType_COST_CATEGORY,
        GroupDefinitionType_DIMENSION,
        GroupDefinitionType_TAG
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype GroupDefinitionType = GroupDefinitionType'
  { fromGroupDefinitionType ::
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

pattern GroupDefinitionType_COST_CATEGORY :: GroupDefinitionType
pattern GroupDefinitionType_COST_CATEGORY = GroupDefinitionType' "COST_CATEGORY"

pattern GroupDefinitionType_DIMENSION :: GroupDefinitionType
pattern GroupDefinitionType_DIMENSION = GroupDefinitionType' "DIMENSION"

pattern GroupDefinitionType_TAG :: GroupDefinitionType
pattern GroupDefinitionType_TAG = GroupDefinitionType' "TAG"

{-# COMPLETE
  GroupDefinitionType_COST_CATEGORY,
  GroupDefinitionType_DIMENSION,
  GroupDefinitionType_TAG,
  GroupDefinitionType'
  #-}
