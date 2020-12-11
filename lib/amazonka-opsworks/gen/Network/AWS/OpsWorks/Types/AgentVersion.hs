-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.AgentVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.AgentVersion
  ( AgentVersion (..),

    -- * Smart constructor
    mkAgentVersion,

    -- * Lenses
    avVersion,
    avConfigurationManager,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types.StackConfigurationManager
import qualified Network.AWS.Prelude as Lude

-- | Describes an agent version.
--
-- /See:/ 'mkAgentVersion' smart constructor.
data AgentVersion = AgentVersion'
  { version :: Lude.Maybe Lude.Text,
    configurationManager :: Lude.Maybe StackConfigurationManager
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AgentVersion' with the minimum fields required to make a request.
--
-- * 'configurationManager' - The configuration manager.
-- * 'version' - The agent version.
mkAgentVersion ::
  AgentVersion
mkAgentVersion =
  AgentVersion'
    { version = Lude.Nothing,
      configurationManager = Lude.Nothing
    }

-- | The agent version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avVersion :: Lens.Lens' AgentVersion (Lude.Maybe Lude.Text)
avVersion = Lens.lens (version :: AgentVersion -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: AgentVersion)
{-# DEPRECATED avVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The configuration manager.
--
-- /Note:/ Consider using 'configurationManager' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avConfigurationManager :: Lens.Lens' AgentVersion (Lude.Maybe StackConfigurationManager)
avConfigurationManager = Lens.lens (configurationManager :: AgentVersion -> Lude.Maybe StackConfigurationManager) (\s a -> s {configurationManager = a} :: AgentVersion)
{-# DEPRECATED avConfigurationManager "Use generic-lens or generic-optics with 'configurationManager' instead." #-}

instance Lude.FromJSON AgentVersion where
  parseJSON =
    Lude.withObject
      "AgentVersion"
      ( \x ->
          AgentVersion'
            Lude.<$> (x Lude..:? "Version")
            Lude.<*> (x Lude..:? "ConfigurationManager")
      )
