{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.DomainEndpointOptionsStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.DomainEndpointOptionsStatus
  ( DomainEndpointOptionsStatus (..),

    -- * Smart constructor
    mkDomainEndpointOptionsStatus,

    -- * Lenses
    deosOptions,
    deosStatus,
  )
where

import qualified Network.AWS.ElasticSearch.Types.DomainEndpointOptions as Types
import qualified Network.AWS.ElasticSearch.Types.OptionStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The configured endpoint options for the domain and their current status.
--
-- /See:/ 'mkDomainEndpointOptionsStatus' smart constructor.
data DomainEndpointOptionsStatus = DomainEndpointOptionsStatus'
  { -- | Options to configure endpoint for the Elasticsearch domain.
    options :: Types.DomainEndpointOptions,
    -- | The status of the endpoint options for the Elasticsearch domain. See @OptionStatus@ for the status information that's included.
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

-- | Options to configure endpoint for the Elasticsearch domain.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deosOptions :: Lens.Lens' DomainEndpointOptionsStatus Types.DomainEndpointOptions
deosOptions = Lens.field @"options"
{-# DEPRECATED deosOptions "Use generic-lens or generic-optics with 'options' instead." #-}

-- | The status of the endpoint options for the Elasticsearch domain. See @OptionStatus@ for the status information that's included.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deosStatus :: Lens.Lens' DomainEndpointOptionsStatus Types.OptionStatus
deosStatus = Lens.field @"status"
{-# DEPRECATED deosStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON DomainEndpointOptionsStatus where
  parseJSON =
    Core.withObject "DomainEndpointOptionsStatus" Core.$
      \x ->
        DomainEndpointOptionsStatus'
          Core.<$> (x Core..: "Options") Core.<*> (x Core..: "Status")
