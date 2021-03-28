{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.ActionSubType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Budgets.Types.ActionSubType
  ( ActionSubType
    ( ActionSubType'
    , ActionSubTypeStopEC2Instances
    , ActionSubTypeStopRdsInstances
    , fromActionSubType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ActionSubType = ActionSubType'{fromActionSubType ::
                                       Core.Text}
                          deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                          Core.Generic)
                          deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                            Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                            Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                            Core.FromText, Core.ToByteString, Core.ToQuery,
                                            Core.ToHeader)

pattern ActionSubTypeStopEC2Instances :: ActionSubType
pattern ActionSubTypeStopEC2Instances = ActionSubType' "STOP_EC2_INSTANCES"

pattern ActionSubTypeStopRdsInstances :: ActionSubType
pattern ActionSubTypeStopRdsInstances = ActionSubType' "STOP_RDS_INSTANCES"

{-# COMPLETE 
  ActionSubTypeStopEC2Instances,

  ActionSubTypeStopRdsInstances,
  ActionSubType'
  #-}
