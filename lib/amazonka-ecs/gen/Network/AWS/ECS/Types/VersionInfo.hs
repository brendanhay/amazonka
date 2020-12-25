{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.VersionInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.VersionInfo
  ( VersionInfo (..),

    -- * Smart constructor
    mkVersionInfo,

    -- * Lenses
    viAgentHash,
    viAgentVersion,
    viDockerVersion,
  )
where

import qualified Network.AWS.ECS.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The Docker and Amazon ECS container agent version information about a container instance.
--
-- /See:/ 'mkVersionInfo' smart constructor.
data VersionInfo = VersionInfo'
  { -- | The Git commit hash for the Amazon ECS container agent build on the <https://github.com/aws/amazon-ecs-agent/commits/master amazon-ecs-agent > GitHub repository.
    agentHash :: Core.Maybe Types.String,
    -- | The version number of the Amazon ECS container agent.
    agentVersion :: Core.Maybe Types.String,
    -- | The Docker version running on the container instance.
    dockerVersion :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VersionInfo' value with any optional fields omitted.
mkVersionInfo ::
  VersionInfo
mkVersionInfo =
  VersionInfo'
    { agentHash = Core.Nothing,
      agentVersion = Core.Nothing,
      dockerVersion = Core.Nothing
    }

-- | The Git commit hash for the Amazon ECS container agent build on the <https://github.com/aws/amazon-ecs-agent/commits/master amazon-ecs-agent > GitHub repository.
--
-- /Note:/ Consider using 'agentHash' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viAgentHash :: Lens.Lens' VersionInfo (Core.Maybe Types.String)
viAgentHash = Lens.field @"agentHash"
{-# DEPRECATED viAgentHash "Use generic-lens or generic-optics with 'agentHash' instead." #-}

-- | The version number of the Amazon ECS container agent.
--
-- /Note:/ Consider using 'agentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viAgentVersion :: Lens.Lens' VersionInfo (Core.Maybe Types.String)
viAgentVersion = Lens.field @"agentVersion"
{-# DEPRECATED viAgentVersion "Use generic-lens or generic-optics with 'agentVersion' instead." #-}

-- | The Docker version running on the container instance.
--
-- /Note:/ Consider using 'dockerVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viDockerVersion :: Lens.Lens' VersionInfo (Core.Maybe Types.String)
viDockerVersion = Lens.field @"dockerVersion"
{-# DEPRECATED viDockerVersion "Use generic-lens or generic-optics with 'dockerVersion' instead." #-}

instance Core.FromJSON VersionInfo where
  toJSON VersionInfo {..} =
    Core.object
      ( Core.catMaybes
          [ ("agentHash" Core..=) Core.<$> agentHash,
            ("agentVersion" Core..=) Core.<$> agentVersion,
            ("dockerVersion" Core..=) Core.<$> dockerVersion
          ]
      )

instance Core.FromJSON VersionInfo where
  parseJSON =
    Core.withObject "VersionInfo" Core.$
      \x ->
        VersionInfo'
          Core.<$> (x Core..:? "agentHash")
          Core.<*> (x Core..:? "agentVersion")
          Core.<*> (x Core..:? "dockerVersion")
