{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.Types.SingleMasterConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisVideo.Types.SingleMasterConfiguration
  ( SingleMasterConfiguration (..)
  -- * Smart constructor
  , mkSingleMasterConfiguration
  -- * Lenses
  , smcMessageTtlSeconds
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A structure that contains the configuration for the @SINGLE_MASTER@ channel type.
--
-- /See:/ 'mkSingleMasterConfiguration' smart constructor.
newtype SingleMasterConfiguration = SingleMasterConfiguration'
  { messageTtlSeconds :: Core.Maybe Core.Natural
    -- ^ The period of time a signaling channel retains underlivered messages before they are discarded.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SingleMasterConfiguration' value with any optional fields omitted.
mkSingleMasterConfiguration
    :: SingleMasterConfiguration
mkSingleMasterConfiguration
  = SingleMasterConfiguration'{messageTtlSeconds = Core.Nothing}

-- | The period of time a signaling channel retains underlivered messages before they are discarded.
--
-- /Note:/ Consider using 'messageTtlSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smcMessageTtlSeconds :: Lens.Lens' SingleMasterConfiguration (Core.Maybe Core.Natural)
smcMessageTtlSeconds = Lens.field @"messageTtlSeconds"
{-# INLINEABLE smcMessageTtlSeconds #-}
{-# DEPRECATED messageTtlSeconds "Use generic-lens or generic-optics with 'messageTtlSeconds' instead"  #-}

instance Core.FromJSON SingleMasterConfiguration where
        toJSON SingleMasterConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [("MessageTtlSeconds" Core..=) Core.<$> messageTtlSeconds])

instance Core.FromJSON SingleMasterConfiguration where
        parseJSON
          = Core.withObject "SingleMasterConfiguration" Core.$
              \ x ->
                SingleMasterConfiguration' Core.<$>
                  (x Core..:? "MessageTtlSeconds")
