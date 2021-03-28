{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.ConfigurationId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MQ.Types.ConfigurationId
  ( ConfigurationId (..)
  -- * Smart constructor
  , mkConfigurationId
  -- * Lenses
  , ciId
  , ciRevision
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | /Important:/ Does not apply to RabbitMQ brokers.
--
-- /See:/ 'mkConfigurationId' smart constructor.
data ConfigurationId = ConfigurationId'
  { id :: Core.Maybe Core.Text
    -- ^ Required. The unique ID that Amazon MQ generates for the configuration.
  , revision :: Core.Maybe Core.Int
    -- ^ The revision number of the configuration.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ConfigurationId' value with any optional fields omitted.
mkConfigurationId
    :: ConfigurationId
mkConfigurationId
  = ConfigurationId'{id = Core.Nothing, revision = Core.Nothing}

-- | Required. The unique ID that Amazon MQ generates for the configuration.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciId :: Lens.Lens' ConfigurationId (Core.Maybe Core.Text)
ciId = Lens.field @"id"
{-# INLINEABLE ciId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The revision number of the configuration.
--
-- /Note:/ Consider using 'revision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciRevision :: Lens.Lens' ConfigurationId (Core.Maybe Core.Int)
ciRevision = Lens.field @"revision"
{-# INLINEABLE ciRevision #-}
{-# DEPRECATED revision "Use generic-lens or generic-optics with 'revision' instead"  #-}

instance Core.FromJSON ConfigurationId where
        toJSON ConfigurationId{..}
          = Core.object
              (Core.catMaybes
                 [("id" Core..=) Core.<$> id,
                  ("revision" Core..=) Core.<$> revision])

instance Core.FromJSON ConfigurationId where
        parseJSON
          = Core.withObject "ConfigurationId" Core.$
              \ x ->
                ConfigurationId' Core.<$>
                  (x Core..:? "id") Core.<*> x Core..:? "revision"
