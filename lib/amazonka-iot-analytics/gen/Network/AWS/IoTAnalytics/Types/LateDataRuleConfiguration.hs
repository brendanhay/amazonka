{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.LateDataRuleConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.LateDataRuleConfiguration
  ( LateDataRuleConfiguration (..),

    -- * Smart constructor
    mkLateDataRuleConfiguration,

    -- * Lenses
    ldrcDeltaTimeSessionWindowConfiguration,
  )
where

import Network.AWS.IoTAnalytics.Types.DeltaTimeSessionWindowConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The information needed to configure a delta time session window.
--
-- /See:/ 'mkLateDataRuleConfiguration' smart constructor.
newtype LateDataRuleConfiguration = LateDataRuleConfiguration'
  { -- | The information needed to configure a delta time session window.
    deltaTimeSessionWindowConfiguration :: Lude.Maybe DeltaTimeSessionWindowConfiguration
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LateDataRuleConfiguration' with the minimum fields required to make a request.
--
-- * 'deltaTimeSessionWindowConfiguration' - The information needed to configure a delta time session window.
mkLateDataRuleConfiguration ::
  LateDataRuleConfiguration
mkLateDataRuleConfiguration =
  LateDataRuleConfiguration'
    { deltaTimeSessionWindowConfiguration =
        Lude.Nothing
    }

-- | The information needed to configure a delta time session window.
--
-- /Note:/ Consider using 'deltaTimeSessionWindowConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrcDeltaTimeSessionWindowConfiguration :: Lens.Lens' LateDataRuleConfiguration (Lude.Maybe DeltaTimeSessionWindowConfiguration)
ldrcDeltaTimeSessionWindowConfiguration = Lens.lens (deltaTimeSessionWindowConfiguration :: LateDataRuleConfiguration -> Lude.Maybe DeltaTimeSessionWindowConfiguration) (\s a -> s {deltaTimeSessionWindowConfiguration = a} :: LateDataRuleConfiguration)
{-# DEPRECATED ldrcDeltaTimeSessionWindowConfiguration "Use generic-lens or generic-optics with 'deltaTimeSessionWindowConfiguration' instead." #-}

instance Lude.FromJSON LateDataRuleConfiguration where
  parseJSON =
    Lude.withObject
      "LateDataRuleConfiguration"
      ( \x ->
          LateDataRuleConfiguration'
            Lude.<$> (x Lude..:? "deltaTimeSessionWindowConfiguration")
      )

instance Lude.ToJSON LateDataRuleConfiguration where
  toJSON LateDataRuleConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("deltaTimeSessionWindowConfiguration" Lude..=)
              Lude.<$> deltaTimeSessionWindowConfiguration
          ]
      )
