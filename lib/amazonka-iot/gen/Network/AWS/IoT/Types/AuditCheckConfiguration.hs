{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuditCheckConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.AuditCheckConfiguration
  ( AuditCheckConfiguration (..)
  -- * Smart constructor
  , mkAuditCheckConfiguration
  -- * Lenses
  , accEnabled
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Which audit checks are enabled and disabled for this account.
--
-- /See:/ 'mkAuditCheckConfiguration' smart constructor.
newtype AuditCheckConfiguration = AuditCheckConfiguration'
  { enabled :: Core.Maybe Core.Bool
    -- ^ True if this audit check is enabled for this account.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AuditCheckConfiguration' value with any optional fields omitted.
mkAuditCheckConfiguration
    :: AuditCheckConfiguration
mkAuditCheckConfiguration
  = AuditCheckConfiguration'{enabled = Core.Nothing}

-- | True if this audit check is enabled for this account.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
accEnabled :: Lens.Lens' AuditCheckConfiguration (Core.Maybe Core.Bool)
accEnabled = Lens.field @"enabled"
{-# INLINEABLE accEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

instance Core.FromJSON AuditCheckConfiguration where
        toJSON AuditCheckConfiguration{..}
          = Core.object
              (Core.catMaybes [("enabled" Core..=) Core.<$> enabled])

instance Core.FromJSON AuditCheckConfiguration where
        parseJSON
          = Core.withObject "AuditCheckConfiguration" Core.$
              \ x -> AuditCheckConfiguration' Core.<$> (x Core..:? "enabled")
