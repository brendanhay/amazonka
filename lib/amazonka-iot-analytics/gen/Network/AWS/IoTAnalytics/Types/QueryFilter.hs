{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.QueryFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.QueryFilter
  ( QueryFilter (..),

    -- * Smart constructor
    mkQueryFilter,

    -- * Lenses
    qfDeltaTime,
  )
where

import Network.AWS.IoTAnalytics.Types.DeltaTime
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information that is used to filter message data, to segregate it according to the timeframe in which it arrives.
--
-- /See:/ 'mkQueryFilter' smart constructor.
newtype QueryFilter = QueryFilter'
  { -- | Used to limit data to that which has arrived since the last execution of the action.
    deltaTime :: Lude.Maybe DeltaTime
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'QueryFilter' with the minimum fields required to make a request.
--
-- * 'deltaTime' - Used to limit data to that which has arrived since the last execution of the action.
mkQueryFilter ::
  QueryFilter
mkQueryFilter = QueryFilter' {deltaTime = Lude.Nothing}

-- | Used to limit data to that which has arrived since the last execution of the action.
--
-- /Note:/ Consider using 'deltaTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qfDeltaTime :: Lens.Lens' QueryFilter (Lude.Maybe DeltaTime)
qfDeltaTime = Lens.lens (deltaTime :: QueryFilter -> Lude.Maybe DeltaTime) (\s a -> s {deltaTime = a} :: QueryFilter)
{-# DEPRECATED qfDeltaTime "Use generic-lens or generic-optics with 'deltaTime' instead." #-}

instance Lude.FromJSON QueryFilter where
  parseJSON =
    Lude.withObject
      "QueryFilter"
      (\x -> QueryFilter' Lude.<$> (x Lude..:? "deltaTime"))

instance Lude.ToJSON QueryFilter where
  toJSON QueryFilter' {..} =
    Lude.object
      (Lude.catMaybes [("deltaTime" Lude..=) Lude.<$> deltaTime])
