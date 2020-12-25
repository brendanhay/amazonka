{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.ServerGroupValidationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.ServerGroupValidationConfiguration
  ( ServerGroupValidationConfiguration (..),

    -- * Smart constructor
    mkServerGroupValidationConfiguration,

    -- * Lenses
    sgvcServerGroupId,
    sgvcServerValidationConfigurations,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SMS.Types.ServerGroupId as Types
import qualified Network.AWS.SMS.Types.ServerValidationConfiguration as Types

-- | Configuration for validating an instance.
--
-- /See:/ 'mkServerGroupValidationConfiguration' smart constructor.
data ServerGroupValidationConfiguration = ServerGroupValidationConfiguration'
  { -- | The ID of the server group.
    serverGroupId :: Core.Maybe Types.ServerGroupId,
    -- | The validation configuration.
    serverValidationConfigurations :: Core.Maybe [Types.ServerValidationConfiguration]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ServerGroupValidationConfiguration' value with any optional fields omitted.
mkServerGroupValidationConfiguration ::
  ServerGroupValidationConfiguration
mkServerGroupValidationConfiguration =
  ServerGroupValidationConfiguration'
    { serverGroupId = Core.Nothing,
      serverValidationConfigurations = Core.Nothing
    }

-- | The ID of the server group.
--
-- /Note:/ Consider using 'serverGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgvcServerGroupId :: Lens.Lens' ServerGroupValidationConfiguration (Core.Maybe Types.ServerGroupId)
sgvcServerGroupId = Lens.field @"serverGroupId"
{-# DEPRECATED sgvcServerGroupId "Use generic-lens or generic-optics with 'serverGroupId' instead." #-}

-- | The validation configuration.
--
-- /Note:/ Consider using 'serverValidationConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgvcServerValidationConfigurations :: Lens.Lens' ServerGroupValidationConfiguration (Core.Maybe [Types.ServerValidationConfiguration])
sgvcServerValidationConfigurations = Lens.field @"serverValidationConfigurations"
{-# DEPRECATED sgvcServerValidationConfigurations "Use generic-lens or generic-optics with 'serverValidationConfigurations' instead." #-}

instance Core.FromJSON ServerGroupValidationConfiguration where
  toJSON ServerGroupValidationConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ ("serverGroupId" Core..=) Core.<$> serverGroupId,
            ("serverValidationConfigurations" Core..=)
              Core.<$> serverValidationConfigurations
          ]
      )

instance Core.FromJSON ServerGroupValidationConfiguration where
  parseJSON =
    Core.withObject "ServerGroupValidationConfiguration" Core.$
      \x ->
        ServerGroupValidationConfiguration'
          Core.<$> (x Core..:? "serverGroupId")
          Core.<*> (x Core..:? "serverValidationConfigurations")
