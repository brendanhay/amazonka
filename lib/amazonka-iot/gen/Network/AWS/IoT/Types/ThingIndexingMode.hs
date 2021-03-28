{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ThingIndexingMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.ThingIndexingMode
  ( ThingIndexingMode
    ( ThingIndexingMode'
    , ThingIndexingModeOff
    , ThingIndexingModeRegistry
    , ThingIndexingModeRegistryAndShadow
    , fromThingIndexingMode
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ThingIndexingMode = ThingIndexingMode'{fromThingIndexingMode
                                               :: Core.Text}
                              deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                              Core.Generic)
                              deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                Core.FromJSON, Core.ToXML, Core.FromXML,
                                                Core.ToText, Core.FromText, Core.ToByteString,
                                                Core.ToQuery, Core.ToHeader)

pattern ThingIndexingModeOff :: ThingIndexingMode
pattern ThingIndexingModeOff = ThingIndexingMode' "OFF"

pattern ThingIndexingModeRegistry :: ThingIndexingMode
pattern ThingIndexingModeRegistry = ThingIndexingMode' "REGISTRY"

pattern ThingIndexingModeRegistryAndShadow :: ThingIndexingMode
pattern ThingIndexingModeRegistryAndShadow = ThingIndexingMode' "REGISTRY_AND_SHADOW"

{-# COMPLETE 
  ThingIndexingModeOff,

  ThingIndexingModeRegistry,

  ThingIndexingModeRegistryAndShadow,
  ThingIndexingMode'
  #-}
