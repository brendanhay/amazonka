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
-- Module      : Amazonka.WellArchitected.Types.DefinitionType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.DefinitionType
  ( DefinitionType
      ( ..,
        DefinitionType_APP_REGISTRY,
        DefinitionType_WORKLOAD_METADATA
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DefinitionType = DefinitionType'
  { fromDefinitionType ::
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

pattern DefinitionType_APP_REGISTRY :: DefinitionType
pattern DefinitionType_APP_REGISTRY = DefinitionType' "APP_REGISTRY"

pattern DefinitionType_WORKLOAD_METADATA :: DefinitionType
pattern DefinitionType_WORKLOAD_METADATA = DefinitionType' "WORKLOAD_METADATA"

{-# COMPLETE
  DefinitionType_APP_REGISTRY,
  DefinitionType_WORKLOAD_METADATA,
  DefinitionType'
  #-}
