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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ThingIndexingMode = ThingIndexingMode'
  { fromThingIndexingMode ::
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
