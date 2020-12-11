-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.NielsenConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.NielsenConfiguration
  ( NielsenConfiguration (..),

    -- * Smart constructor
    mkNielsenConfiguration,

    -- * Lenses
    ncDistributorId,
    ncNielsenPcmToId3Tagging,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.NielsenPcmToId3TaggingState
import qualified Network.AWS.Prelude as Lude

-- | Nielsen Configuration
--
-- /See:/ 'mkNielsenConfiguration' smart constructor.
data NielsenConfiguration = NielsenConfiguration'
  { distributorId ::
      Lude.Maybe Lude.Text,
    nielsenPcmToId3Tagging ::
      Lude.Maybe NielsenPcmToId3TaggingState
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NielsenConfiguration' with the minimum fields required to make a request.
--
-- * 'distributorId' - Enter the Distributor ID assigned to your organization by Nielsen.
-- * 'nielsenPcmToId3Tagging' - Enables Nielsen PCM to ID3 tagging
mkNielsenConfiguration ::
  NielsenConfiguration
mkNielsenConfiguration =
  NielsenConfiguration'
    { distributorId = Lude.Nothing,
      nielsenPcmToId3Tagging = Lude.Nothing
    }

-- | Enter the Distributor ID assigned to your organization by Nielsen.
--
-- /Note:/ Consider using 'distributorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncDistributorId :: Lens.Lens' NielsenConfiguration (Lude.Maybe Lude.Text)
ncDistributorId = Lens.lens (distributorId :: NielsenConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {distributorId = a} :: NielsenConfiguration)
{-# DEPRECATED ncDistributorId "Use generic-lens or generic-optics with 'distributorId' instead." #-}

-- | Enables Nielsen PCM to ID3 tagging
--
-- /Note:/ Consider using 'nielsenPcmToId3Tagging' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncNielsenPcmToId3Tagging :: Lens.Lens' NielsenConfiguration (Lude.Maybe NielsenPcmToId3TaggingState)
ncNielsenPcmToId3Tagging = Lens.lens (nielsenPcmToId3Tagging :: NielsenConfiguration -> Lude.Maybe NielsenPcmToId3TaggingState) (\s a -> s {nielsenPcmToId3Tagging = a} :: NielsenConfiguration)
{-# DEPRECATED ncNielsenPcmToId3Tagging "Use generic-lens or generic-optics with 'nielsenPcmToId3Tagging' instead." #-}

instance Lude.FromJSON NielsenConfiguration where
  parseJSON =
    Lude.withObject
      "NielsenConfiguration"
      ( \x ->
          NielsenConfiguration'
            Lude.<$> (x Lude..:? "distributorId")
            Lude.<*> (x Lude..:? "nielsenPcmToId3Tagging")
      )

instance Lude.ToJSON NielsenConfiguration where
  toJSON NielsenConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("distributorId" Lude..=) Lude.<$> distributorId,
            ("nielsenPcmToId3Tagging" Lude..=)
              Lude.<$> nielsenPcmToId3Tagging
          ]
      )
