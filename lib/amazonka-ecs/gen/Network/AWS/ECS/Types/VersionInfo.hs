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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The Docker and Amazon ECS container agent version information about a container instance.
--
-- /See:/ 'mkVersionInfo' smart constructor.
data VersionInfo = VersionInfo'
  { -- | The Git commit hash for the Amazon ECS container agent build on the <https://github.com/aws/amazon-ecs-agent/commits/master amazon-ecs-agent > GitHub repository.
    agentHash :: Lude.Maybe Lude.Text,
    -- | The version number of the Amazon ECS container agent.
    agentVersion :: Lude.Maybe Lude.Text,
    -- | The Docker version running on the container instance.
    dockerVersion :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VersionInfo' with the minimum fields required to make a request.
--
-- * 'agentHash' - The Git commit hash for the Amazon ECS container agent build on the <https://github.com/aws/amazon-ecs-agent/commits/master amazon-ecs-agent > GitHub repository.
-- * 'agentVersion' - The version number of the Amazon ECS container agent.
-- * 'dockerVersion' - The Docker version running on the container instance.
mkVersionInfo ::
  VersionInfo
mkVersionInfo =
  VersionInfo'
    { agentHash = Lude.Nothing,
      agentVersion = Lude.Nothing,
      dockerVersion = Lude.Nothing
    }

-- | The Git commit hash for the Amazon ECS container agent build on the <https://github.com/aws/amazon-ecs-agent/commits/master amazon-ecs-agent > GitHub repository.
--
-- /Note:/ Consider using 'agentHash' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viAgentHash :: Lens.Lens' VersionInfo (Lude.Maybe Lude.Text)
viAgentHash = Lens.lens (agentHash :: VersionInfo -> Lude.Maybe Lude.Text) (\s a -> s {agentHash = a} :: VersionInfo)
{-# DEPRECATED viAgentHash "Use generic-lens or generic-optics with 'agentHash' instead." #-}

-- | The version number of the Amazon ECS container agent.
--
-- /Note:/ Consider using 'agentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viAgentVersion :: Lens.Lens' VersionInfo (Lude.Maybe Lude.Text)
viAgentVersion = Lens.lens (agentVersion :: VersionInfo -> Lude.Maybe Lude.Text) (\s a -> s {agentVersion = a} :: VersionInfo)
{-# DEPRECATED viAgentVersion "Use generic-lens or generic-optics with 'agentVersion' instead." #-}

-- | The Docker version running on the container instance.
--
-- /Note:/ Consider using 'dockerVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viDockerVersion :: Lens.Lens' VersionInfo (Lude.Maybe Lude.Text)
viDockerVersion = Lens.lens (dockerVersion :: VersionInfo -> Lude.Maybe Lude.Text) (\s a -> s {dockerVersion = a} :: VersionInfo)
{-# DEPRECATED viDockerVersion "Use generic-lens or generic-optics with 'dockerVersion' instead." #-}

instance Lude.FromJSON VersionInfo where
  parseJSON =
    Lude.withObject
      "VersionInfo"
      ( \x ->
          VersionInfo'
            Lude.<$> (x Lude..:? "agentHash")
            Lude.<*> (x Lude..:? "agentVersion")
            Lude.<*> (x Lude..:? "dockerVersion")
      )

instance Lude.ToJSON VersionInfo where
  toJSON VersionInfo' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("agentHash" Lude..=) Lude.<$> agentHash,
            ("agentVersion" Lude..=) Lude.<$> agentVersion,
            ("dockerVersion" Lude..=) Lude.<$> dockerVersion
          ]
      )
