{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.Types.SSESpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DAX.Types.SSESpecification
  ( SSESpecification (..),

    -- * Smart constructor
    mkSSESpecification,

    -- * Lenses
    ssesEnabled,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the settings used to enable server-side encryption.
--
-- /See:/ 'mkSSESpecification' smart constructor.
newtype SSESpecification = SSESpecification'
  { -- | Indicates whether server-side encryption is enabled (true) or disabled (false) on the cluster.
    enabled :: Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SSESpecification' with the minimum fields required to make a request.
--
-- * 'enabled' - Indicates whether server-side encryption is enabled (true) or disabled (false) on the cluster.
mkSSESpecification ::
  -- | 'enabled'
  Lude.Bool ->
  SSESpecification
mkSSESpecification pEnabled_ =
  SSESpecification' {enabled = pEnabled_}

-- | Indicates whether server-side encryption is enabled (true) or disabled (false) on the cluster.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssesEnabled :: Lens.Lens' SSESpecification Lude.Bool
ssesEnabled = Lens.lens (enabled :: SSESpecification -> Lude.Bool) (\s a -> s {enabled = a} :: SSESpecification)
{-# DEPRECATED ssesEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

instance Lude.ToJSON SSESpecification where
  toJSON SSESpecification' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("Enabled" Lude..= enabled)])
