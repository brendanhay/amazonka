-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.VersioningConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.VersioningConfiguration
  ( VersioningConfiguration (..),

    -- * Smart constructor
    mkVersioningConfiguration,

    -- * Lenses
    vcUnlimited,
    vcMaxVersions,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the versioning of dataset contents.
--
-- /See:/ 'mkVersioningConfiguration' smart constructor.
data VersioningConfiguration = VersioningConfiguration'
  { unlimited ::
      Lude.Maybe Lude.Bool,
    maxVersions :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VersioningConfiguration' with the minimum fields required to make a request.
--
-- * 'maxVersions' - How many versions of dataset contents are kept. The @unlimited@ parameter must be @false@ .
-- * 'unlimited' - If true, unlimited versions of dataset contents are kept.
mkVersioningConfiguration ::
  VersioningConfiguration
mkVersioningConfiguration =
  VersioningConfiguration'
    { unlimited = Lude.Nothing,
      maxVersions = Lude.Nothing
    }

-- | If true, unlimited versions of dataset contents are kept.
--
-- /Note:/ Consider using 'unlimited' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcUnlimited :: Lens.Lens' VersioningConfiguration (Lude.Maybe Lude.Bool)
vcUnlimited = Lens.lens (unlimited :: VersioningConfiguration -> Lude.Maybe Lude.Bool) (\s a -> s {unlimited = a} :: VersioningConfiguration)
{-# DEPRECATED vcUnlimited "Use generic-lens or generic-optics with 'unlimited' instead." #-}

-- | How many versions of dataset contents are kept. The @unlimited@ parameter must be @false@ .
--
-- /Note:/ Consider using 'maxVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcMaxVersions :: Lens.Lens' VersioningConfiguration (Lude.Maybe Lude.Natural)
vcMaxVersions = Lens.lens (maxVersions :: VersioningConfiguration -> Lude.Maybe Lude.Natural) (\s a -> s {maxVersions = a} :: VersioningConfiguration)
{-# DEPRECATED vcMaxVersions "Use generic-lens or generic-optics with 'maxVersions' instead." #-}

instance Lude.FromJSON VersioningConfiguration where
  parseJSON =
    Lude.withObject
      "VersioningConfiguration"
      ( \x ->
          VersioningConfiguration'
            Lude.<$> (x Lude..:? "unlimited") Lude.<*> (x Lude..:? "maxVersions")
      )

instance Lude.ToJSON VersioningConfiguration where
  toJSON VersioningConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("unlimited" Lude..=) Lude.<$> unlimited,
            ("maxVersions" Lude..=) Lude.<$> maxVersions
          ]
      )
