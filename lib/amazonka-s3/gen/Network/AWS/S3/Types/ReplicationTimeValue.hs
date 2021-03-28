{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ReplicationTimeValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.ReplicationTimeValue
  ( ReplicationTimeValue (..)
  -- * Smart constructor
  , mkReplicationTimeValue
  -- * Lenses
  , rtvMinutes
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types

-- | A container specifying the time value for S3 Replication Time Control (S3 RTC) and replication metrics @EventThreshold@ . 
--
-- /See:/ 'mkReplicationTimeValue' smart constructor.
newtype ReplicationTimeValue = ReplicationTimeValue'
  { minutes :: Core.Maybe Core.Int
    -- ^ Contains an integer specifying time in minutes. 
--
-- Valid values: 15 minutes. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ReplicationTimeValue' value with any optional fields omitted.
mkReplicationTimeValue
    :: ReplicationTimeValue
mkReplicationTimeValue
  = ReplicationTimeValue'{minutes = Core.Nothing}

-- | Contains an integer specifying time in minutes. 
--
-- Valid values: 15 minutes. 
--
-- /Note:/ Consider using 'minutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtvMinutes :: Lens.Lens' ReplicationTimeValue (Core.Maybe Core.Int)
rtvMinutes = Lens.field @"minutes"
{-# INLINEABLE rtvMinutes #-}
{-# DEPRECATED minutes "Use generic-lens or generic-optics with 'minutes' instead"  #-}

instance Core.ToXML ReplicationTimeValue where
        toXML ReplicationTimeValue{..}
          = Core.maybe Core.mempty (Core.toXMLElement "Minutes") minutes

instance Core.FromXML ReplicationTimeValue where
        parseXML x = ReplicationTimeValue' Core.<$> (x Core..@? "Minutes")
