{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.ReplicationRunType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SMS.Types.ReplicationRunType
  ( ReplicationRunType
    ( ReplicationRunType'
    , ReplicationRunTypeOnDemand
    , ReplicationRunTypeAutomatic
    , fromReplicationRunType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ReplicationRunType = ReplicationRunType'{fromReplicationRunType
                                                 :: Core.Text}
                               deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                               Core.Generic)
                               deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                 Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                 Core.FromJSON, Core.ToXML, Core.FromXML,
                                                 Core.ToText, Core.FromText, Core.ToByteString,
                                                 Core.ToQuery, Core.ToHeader)

pattern ReplicationRunTypeOnDemand :: ReplicationRunType
pattern ReplicationRunTypeOnDemand = ReplicationRunType' "ON_DEMAND"

pattern ReplicationRunTypeAutomatic :: ReplicationRunType
pattern ReplicationRunTypeAutomatic = ReplicationRunType' "AUTOMATIC"

{-# COMPLETE 
  ReplicationRunTypeOnDemand,

  ReplicationRunTypeAutomatic,
  ReplicationRunType'
  #-}
