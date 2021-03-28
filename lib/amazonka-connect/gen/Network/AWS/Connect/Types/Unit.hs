{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.Unit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Connect.Types.Unit
  ( Unit
    ( Unit'
    , UnitSeconds
    , UnitCount
    , UnitPercent
    , fromUnit
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype Unit = Unit'{fromUnit :: Core.Text}
                 deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                 Core.Generic)
                 deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                   Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON, Core.FromJSON,
                                   Core.ToXML, Core.FromXML, Core.ToText, Core.FromText,
                                   Core.ToByteString, Core.ToQuery, Core.ToHeader)

pattern UnitSeconds :: Unit
pattern UnitSeconds = Unit' "SECONDS"

pattern UnitCount :: Unit
pattern UnitCount = Unit' "COUNT"

pattern UnitPercent :: Unit
pattern UnitPercent = Unit' "PERCENT"

{-# COMPLETE 
  UnitSeconds,

  UnitCount,

  UnitPercent,
  Unit'
  #-}
