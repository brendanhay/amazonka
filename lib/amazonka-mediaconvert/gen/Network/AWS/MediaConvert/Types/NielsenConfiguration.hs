{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.NielsenConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.NielsenConfiguration
  ( NielsenConfiguration (..),

    -- * Smart constructor
    mkNielsenConfiguration,

    -- * Lenses
    ncBreakoutCode,
    ncDistributorId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Settings for your Nielsen configuration. If you don't do Nielsen measurement and analytics, ignore these settings. When you enable Nielsen configuration (nielsenConfiguration), MediaConvert enables PCM to ID3 tagging for all outputs in the job. To enable Nielsen configuration programmatically, include an instance of nielsenConfiguration in your JSON job specification. Even if you don't include any children of nielsenConfiguration, you still enable the setting.
--
-- /See:/ 'mkNielsenConfiguration' smart constructor.
data NielsenConfiguration = NielsenConfiguration'
  { -- | Nielsen has discontinued the use of breakout code functionality. If you must include this property, set the value to zero.
    breakoutCode :: Lude.Maybe Lude.Natural,
    -- | Use Distributor ID (DistributorID) to specify the distributor ID that is assigned to your organization by Neilsen.
    distributorId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NielsenConfiguration' with the minimum fields required to make a request.
--
-- * 'breakoutCode' - Nielsen has discontinued the use of breakout code functionality. If you must include this property, set the value to zero.
-- * 'distributorId' - Use Distributor ID (DistributorID) to specify the distributor ID that is assigned to your organization by Neilsen.
mkNielsenConfiguration ::
  NielsenConfiguration
mkNielsenConfiguration =
  NielsenConfiguration'
    { breakoutCode = Lude.Nothing,
      distributorId = Lude.Nothing
    }

-- | Nielsen has discontinued the use of breakout code functionality. If you must include this property, set the value to zero.
--
-- /Note:/ Consider using 'breakoutCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncBreakoutCode :: Lens.Lens' NielsenConfiguration (Lude.Maybe Lude.Natural)
ncBreakoutCode = Lens.lens (breakoutCode :: NielsenConfiguration -> Lude.Maybe Lude.Natural) (\s a -> s {breakoutCode = a} :: NielsenConfiguration)
{-# DEPRECATED ncBreakoutCode "Use generic-lens or generic-optics with 'breakoutCode' instead." #-}

-- | Use Distributor ID (DistributorID) to specify the distributor ID that is assigned to your organization by Neilsen.
--
-- /Note:/ Consider using 'distributorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncDistributorId :: Lens.Lens' NielsenConfiguration (Lude.Maybe Lude.Text)
ncDistributorId = Lens.lens (distributorId :: NielsenConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {distributorId = a} :: NielsenConfiguration)
{-# DEPRECATED ncDistributorId "Use generic-lens or generic-optics with 'distributorId' instead." #-}

instance Lude.FromJSON NielsenConfiguration where
  parseJSON =
    Lude.withObject
      "NielsenConfiguration"
      ( \x ->
          NielsenConfiguration'
            Lude.<$> (x Lude..:? "breakoutCode") Lude.<*> (x Lude..:? "distributorId")
      )

instance Lude.ToJSON NielsenConfiguration where
  toJSON NielsenConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("breakoutCode" Lude..=) Lude.<$> breakoutCode,
            ("distributorId" Lude..=) Lude.<$> distributorId
          ]
      )
