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
-- Module      : Amazonka.IoTThingsGraph.Types.EntityFilterName
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTThingsGraph.Types.EntityFilterName
  ( EntityFilterName
      ( ..,
        EntityFilterName_NAME,
        EntityFilterName_NAMESPACE,
        EntityFilterName_REFERENCED_ENTITY_ID,
        EntityFilterName_SEMANTIC_TYPE_PATH
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype EntityFilterName = EntityFilterName'
  { fromEntityFilterName ::
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

pattern EntityFilterName_NAME :: EntityFilterName
pattern EntityFilterName_NAME = EntityFilterName' "NAME"

pattern EntityFilterName_NAMESPACE :: EntityFilterName
pattern EntityFilterName_NAMESPACE = EntityFilterName' "NAMESPACE"

pattern EntityFilterName_REFERENCED_ENTITY_ID :: EntityFilterName
pattern EntityFilterName_REFERENCED_ENTITY_ID = EntityFilterName' "REFERENCED_ENTITY_ID"

pattern EntityFilterName_SEMANTIC_TYPE_PATH :: EntityFilterName
pattern EntityFilterName_SEMANTIC_TYPE_PATH = EntityFilterName' "SEMANTIC_TYPE_PATH"

{-# COMPLETE
  EntityFilterName_NAME,
  EntityFilterName_NAMESPACE,
  EntityFilterName_REFERENCED_ENTITY_ID,
  EntityFilterName_SEMANTIC_TYPE_PATH,
  EntityFilterName'
  #-}
