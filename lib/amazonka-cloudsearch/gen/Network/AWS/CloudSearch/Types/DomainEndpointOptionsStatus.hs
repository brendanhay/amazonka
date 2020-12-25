{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.DomainEndpointOptionsStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.DomainEndpointOptionsStatus
  ( DomainEndpointOptionsStatus (..),

    -- * Smart constructor
    mkDomainEndpointOptionsStatus,

    -- * Lenses
    deosOptions,
    deosStatus,
  )
where

import qualified Network.AWS.CloudSearch.Types.DomainEndpointOptions as Types
import qualified Network.AWS.CloudSearch.Types.OptionStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The configuration and status of the domain's endpoint options.
--
-- /See:/ 'mkDomainEndpointOptionsStatus' smart constructor.
data DomainEndpointOptionsStatus = DomainEndpointOptionsStatus'
  { -- | The domain endpoint options configured for the domain.
    options :: Types.DomainEndpointOptions,
    -- | The status of the configured domain endpoint options.
    status :: Types.OptionStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DomainEndpointOptionsStatus' value with any optional fields omitted.
mkDomainEndpointOptionsStatus ::
  -- | 'options'
  Types.DomainEndpointOptions ->
  -- | 'status'
  Types.OptionStatus ->
  DomainEndpointOptionsStatus
mkDomainEndpointOptionsStatus options status =
  DomainEndpointOptionsStatus' {options, status}

-- | The domain endpoint options configured for the domain.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deosOptions :: Lens.Lens' DomainEndpointOptionsStatus Types.DomainEndpointOptions
deosOptions = Lens.field @"options"
{-# DEPRECATED deosOptions "Use generic-lens or generic-optics with 'options' instead." #-}

-- | The status of the configured domain endpoint options.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deosStatus :: Lens.Lens' DomainEndpointOptionsStatus Types.OptionStatus
deosStatus = Lens.field @"status"
{-# DEPRECATED deosStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromXML DomainEndpointOptionsStatus where
  parseXML x =
    DomainEndpointOptionsStatus'
      Core.<$> (x Core..@ "Options") Core.<*> (x Core..@ "Status")
