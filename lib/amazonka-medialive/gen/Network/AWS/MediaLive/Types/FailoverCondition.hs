{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.FailoverCondition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.FailoverCondition
  ( FailoverCondition (..),

    -- * Smart constructor
    mkFailoverCondition,

    -- * Lenses
    fcFailoverConditionSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.FailoverConditionSettings
import qualified Network.AWS.Prelude as Lude

-- | Failover Condition settings. There can be multiple failover conditions inside AutomaticInputFailoverSettings.
--
-- /See:/ 'mkFailoverCondition' smart constructor.
newtype FailoverCondition = FailoverCondition'
  { -- | Failover condition type-specific settings.
    failoverConditionSettings :: Lude.Maybe FailoverConditionSettings
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FailoverCondition' with the minimum fields required to make a request.
--
-- * 'failoverConditionSettings' - Failover condition type-specific settings.
mkFailoverCondition ::
  FailoverCondition
mkFailoverCondition =
  FailoverCondition' {failoverConditionSettings = Lude.Nothing}

-- | Failover condition type-specific settings.
--
-- /Note:/ Consider using 'failoverConditionSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcFailoverConditionSettings :: Lens.Lens' FailoverCondition (Lude.Maybe FailoverConditionSettings)
fcFailoverConditionSettings = Lens.lens (failoverConditionSettings :: FailoverCondition -> Lude.Maybe FailoverConditionSettings) (\s a -> s {failoverConditionSettings = a} :: FailoverCondition)
{-# DEPRECATED fcFailoverConditionSettings "Use generic-lens or generic-optics with 'failoverConditionSettings' instead." #-}

instance Lude.FromJSON FailoverCondition where
  parseJSON =
    Lude.withObject
      "FailoverCondition"
      ( \x ->
          FailoverCondition'
            Lude.<$> (x Lude..:? "failoverConditionSettings")
      )

instance Lude.ToJSON FailoverCondition where
  toJSON FailoverCondition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("failoverConditionSettings" Lude..=)
              Lude.<$> failoverConditionSettings
          ]
      )
