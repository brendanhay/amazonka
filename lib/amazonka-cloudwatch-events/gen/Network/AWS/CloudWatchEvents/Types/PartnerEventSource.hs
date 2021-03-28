{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.PartnerEventSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatchEvents.Types.PartnerEventSource
  ( PartnerEventSource (..)
  -- * Smart constructor
  , mkPartnerEventSource
  -- * Lenses
  , pesArn
  , pesName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A partner event source is created by an SaaS partner. If a customer creates a partner event bus that matches this event source, that AWS account can receive events from the partner's applications or services.
--
-- /See:/ 'mkPartnerEventSource' smart constructor.
data PartnerEventSource = PartnerEventSource'
  { arn :: Core.Maybe Core.Text
    -- ^ The ARN of the partner event source.
  , name :: Core.Maybe Core.Text
    -- ^ The name of the partner event source.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PartnerEventSource' value with any optional fields omitted.
mkPartnerEventSource
    :: PartnerEventSource
mkPartnerEventSource
  = PartnerEventSource'{arn = Core.Nothing, name = Core.Nothing}

-- | The ARN of the partner event source.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pesArn :: Lens.Lens' PartnerEventSource (Core.Maybe Core.Text)
pesArn = Lens.field @"arn"
{-# INLINEABLE pesArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The name of the partner event source.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pesName :: Lens.Lens' PartnerEventSource (Core.Maybe Core.Text)
pesName = Lens.field @"name"
{-# INLINEABLE pesName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromJSON PartnerEventSource where
        parseJSON
          = Core.withObject "PartnerEventSource" Core.$
              \ x ->
                PartnerEventSource' Core.<$>
                  (x Core..:? "Arn") Core.<*> x Core..:? "Name"
