-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.Types.DiscoveredResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MigrationHub.Types.DiscoveredResource
  ( DiscoveredResource (..),

    -- * Smart constructor
    mkDiscoveredResource,

    -- * Lenses
    drDescription,
    drConfigurationId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Object representing the on-premises resource being migrated.
--
-- /See:/ 'mkDiscoveredResource' smart constructor.
data DiscoveredResource = DiscoveredResource'
  { description ::
      Lude.Maybe Lude.Text,
    configurationId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DiscoveredResource' with the minimum fields required to make a request.
--
-- * 'configurationId' - The configurationId in Application Discovery Service that uniquely identifies the on-premise resource.
-- * 'description' - A description that can be free-form text to record additional detail about the discovered resource for clarity or later reference.
mkDiscoveredResource ::
  -- | 'configurationId'
  Lude.Text ->
  DiscoveredResource
mkDiscoveredResource pConfigurationId_ =
  DiscoveredResource'
    { description = Lude.Nothing,
      configurationId = pConfigurationId_
    }

-- | A description that can be free-form text to record additional detail about the discovered resource for clarity or later reference.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drDescription :: Lens.Lens' DiscoveredResource (Lude.Maybe Lude.Text)
drDescription = Lens.lens (description :: DiscoveredResource -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: DiscoveredResource)
{-# DEPRECATED drDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The configurationId in Application Discovery Service that uniquely identifies the on-premise resource.
--
-- /Note:/ Consider using 'configurationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drConfigurationId :: Lens.Lens' DiscoveredResource Lude.Text
drConfigurationId = Lens.lens (configurationId :: DiscoveredResource -> Lude.Text) (\s a -> s {configurationId = a} :: DiscoveredResource)
{-# DEPRECATED drConfigurationId "Use generic-lens or generic-optics with 'configurationId' instead." #-}

instance Lude.FromJSON DiscoveredResource where
  parseJSON =
    Lude.withObject
      "DiscoveredResource"
      ( \x ->
          DiscoveredResource'
            Lude.<$> (x Lude..:? "Description") Lude.<*> (x Lude..: "ConfigurationId")
      )

instance Lude.ToJSON DiscoveredResource where
  toJSON DiscoveredResource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Description" Lude..=) Lude.<$> description,
            Lude.Just ("ConfigurationId" Lude..= configurationId)
          ]
      )
