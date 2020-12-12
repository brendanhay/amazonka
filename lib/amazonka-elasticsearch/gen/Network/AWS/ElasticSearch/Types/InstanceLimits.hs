{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.InstanceLimits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.InstanceLimits
  ( InstanceLimits (..),

    -- * Smart constructor
    mkInstanceLimits,

    -- * Lenses
    ilInstanceCountLimits,
  )
where

import Network.AWS.ElasticSearch.Types.InstanceCountLimits
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | InstanceLimits represents the list of instance related attributes that are available for given InstanceType.
--
-- /See:/ 'mkInstanceLimits' smart constructor.
newtype InstanceLimits = InstanceLimits'
  { instanceCountLimits ::
      Lude.Maybe InstanceCountLimits
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceLimits' with the minimum fields required to make a request.
--
-- * 'instanceCountLimits' - Undocumented field.
mkInstanceLimits ::
  InstanceLimits
mkInstanceLimits =
  InstanceLimits' {instanceCountLimits = Lude.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'instanceCountLimits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ilInstanceCountLimits :: Lens.Lens' InstanceLimits (Lude.Maybe InstanceCountLimits)
ilInstanceCountLimits = Lens.lens (instanceCountLimits :: InstanceLimits -> Lude.Maybe InstanceCountLimits) (\s a -> s {instanceCountLimits = a} :: InstanceLimits)
{-# DEPRECATED ilInstanceCountLimits "Use generic-lens or generic-optics with 'instanceCountLimits' instead." #-}

instance Lude.FromJSON InstanceLimits where
  parseJSON =
    Lude.withObject
      "InstanceLimits"
      ( \x ->
          InstanceLimits' Lude.<$> (x Lude..:? "InstanceCountLimits")
      )
