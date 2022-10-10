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
-- Module      : Amazonka.IoT.Types.ThingIndexingMode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.ThingIndexingMode
  ( ThingIndexingMode
      ( ..,
        ThingIndexingMode_OFF,
        ThingIndexingMode_REGISTRY,
        ThingIndexingMode_REGISTRY_AND_SHADOW
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ThingIndexingMode = ThingIndexingMode'
  { fromThingIndexingMode ::
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

pattern ThingIndexingMode_OFF :: ThingIndexingMode
pattern ThingIndexingMode_OFF = ThingIndexingMode' "OFF"

pattern ThingIndexingMode_REGISTRY :: ThingIndexingMode
pattern ThingIndexingMode_REGISTRY = ThingIndexingMode' "REGISTRY"

pattern ThingIndexingMode_REGISTRY_AND_SHADOW :: ThingIndexingMode
pattern ThingIndexingMode_REGISTRY_AND_SHADOW = ThingIndexingMode' "REGISTRY_AND_SHADOW"

{-# COMPLETE
  ThingIndexingMode_OFF,
  ThingIndexingMode_REGISTRY,
  ThingIndexingMode_REGISTRY_AND_SHADOW,
  ThingIndexingMode'
  #-}
