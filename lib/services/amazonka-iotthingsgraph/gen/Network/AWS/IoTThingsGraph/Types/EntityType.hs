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
-- Module      : Network.AWS.IoTThingsGraph.Types.EntityType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTThingsGraph.Types.EntityType
  ( EntityType
      ( ..,
        EntityType_ACTION,
        EntityType_CAPABILITY,
        EntityType_DEVICE,
        EntityType_DEVICE_MODEL,
        EntityType_ENUM,
        EntityType_EVENT,
        EntityType_MAPPING,
        EntityType_PROPERTY,
        EntityType_SERVICE,
        EntityType_STATE
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype EntityType = EntityType'
  { fromEntityType ::
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

pattern EntityType_ACTION :: EntityType
pattern EntityType_ACTION = EntityType' "ACTION"

pattern EntityType_CAPABILITY :: EntityType
pattern EntityType_CAPABILITY = EntityType' "CAPABILITY"

pattern EntityType_DEVICE :: EntityType
pattern EntityType_DEVICE = EntityType' "DEVICE"

pattern EntityType_DEVICE_MODEL :: EntityType
pattern EntityType_DEVICE_MODEL = EntityType' "DEVICE_MODEL"

pattern EntityType_ENUM :: EntityType
pattern EntityType_ENUM = EntityType' "ENUM"

pattern EntityType_EVENT :: EntityType
pattern EntityType_EVENT = EntityType' "EVENT"

pattern EntityType_MAPPING :: EntityType
pattern EntityType_MAPPING = EntityType' "MAPPING"

pattern EntityType_PROPERTY :: EntityType
pattern EntityType_PROPERTY = EntityType' "PROPERTY"

pattern EntityType_SERVICE :: EntityType
pattern EntityType_SERVICE = EntityType' "SERVICE"

pattern EntityType_STATE :: EntityType
pattern EntityType_STATE = EntityType' "STATE"

{-# COMPLETE
  EntityType_ACTION,
  EntityType_CAPABILITY,
  EntityType_DEVICE,
  EntityType_DEVICE_MODEL,
  EntityType_ENUM,
  EntityType_EVENT,
  EntityType_MAPPING,
  EntityType_PROPERTY,
  EntityType_SERVICE,
  EntityType_STATE,
  EntityType'
  #-}
