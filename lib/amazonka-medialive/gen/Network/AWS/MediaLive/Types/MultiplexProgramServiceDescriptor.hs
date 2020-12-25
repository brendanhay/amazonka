{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MultiplexProgramServiceDescriptor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MultiplexProgramServiceDescriptor
  ( MultiplexProgramServiceDescriptor (..),

    -- * Smart constructor
    mkMultiplexProgramServiceDescriptor,

    -- * Lenses
    mpsdProviderName,
    mpsdServiceName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Transport stream service descriptor configuration for the Multiplex program.
--
-- /See:/ 'mkMultiplexProgramServiceDescriptor' smart constructor.
data MultiplexProgramServiceDescriptor = MultiplexProgramServiceDescriptor'
  { -- | Name of the provider.
    providerName :: Core.Text,
    -- | Name of the service.
    serviceName :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MultiplexProgramServiceDescriptor' value with any optional fields omitted.
mkMultiplexProgramServiceDescriptor ::
  -- | 'providerName'
  Core.Text ->
  -- | 'serviceName'
  Core.Text ->
  MultiplexProgramServiceDescriptor
mkMultiplexProgramServiceDescriptor providerName serviceName =
  MultiplexProgramServiceDescriptor' {providerName, serviceName}

-- | Name of the provider.
--
-- /Note:/ Consider using 'providerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpsdProviderName :: Lens.Lens' MultiplexProgramServiceDescriptor Core.Text
mpsdProviderName = Lens.field @"providerName"
{-# DEPRECATED mpsdProviderName "Use generic-lens or generic-optics with 'providerName' instead." #-}

-- | Name of the service.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpsdServiceName :: Lens.Lens' MultiplexProgramServiceDescriptor Core.Text
mpsdServiceName = Lens.field @"serviceName"
{-# DEPRECATED mpsdServiceName "Use generic-lens or generic-optics with 'serviceName' instead." #-}

instance Core.FromJSON MultiplexProgramServiceDescriptor where
  toJSON MultiplexProgramServiceDescriptor {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("providerName" Core..= providerName),
            Core.Just ("serviceName" Core..= serviceName)
          ]
      )

instance Core.FromJSON MultiplexProgramServiceDescriptor where
  parseJSON =
    Core.withObject "MultiplexProgramServiceDescriptor" Core.$
      \x ->
        MultiplexProgramServiceDescriptor'
          Core.<$> (x Core..: "providerName") Core.<*> (x Core..: "serviceName")
