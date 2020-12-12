{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ResizeInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ResizeInfo
  ( ResizeInfo (..),

    -- * Smart constructor
    mkResizeInfo,

    -- * Lenses
    riAllowCancelResize,
    riResizeType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal

-- | Describes a resize operation.
--
-- /See:/ 'mkResizeInfo' smart constructor.
data ResizeInfo = ResizeInfo'
  { allowCancelResize ::
      Lude.Maybe Lude.Bool,
    resizeType :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResizeInfo' with the minimum fields required to make a request.
--
-- * 'allowCancelResize' - A boolean value indicating if the resize operation can be cancelled.
-- * 'resizeType' - Returns the value @ClassicResize@ .
mkResizeInfo ::
  ResizeInfo
mkResizeInfo =
  ResizeInfo'
    { allowCancelResize = Lude.Nothing,
      resizeType = Lude.Nothing
    }

-- | A boolean value indicating if the resize operation can be cancelled.
--
-- /Note:/ Consider using 'allowCancelResize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riAllowCancelResize :: Lens.Lens' ResizeInfo (Lude.Maybe Lude.Bool)
riAllowCancelResize = Lens.lens (allowCancelResize :: ResizeInfo -> Lude.Maybe Lude.Bool) (\s a -> s {allowCancelResize = a} :: ResizeInfo)
{-# DEPRECATED riAllowCancelResize "Use generic-lens or generic-optics with 'allowCancelResize' instead." #-}

-- | Returns the value @ClassicResize@ .
--
-- /Note:/ Consider using 'resizeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riResizeType :: Lens.Lens' ResizeInfo (Lude.Maybe Lude.Text)
riResizeType = Lens.lens (resizeType :: ResizeInfo -> Lude.Maybe Lude.Text) (\s a -> s {resizeType = a} :: ResizeInfo)
{-# DEPRECATED riResizeType "Use generic-lens or generic-optics with 'resizeType' instead." #-}

instance Lude.FromXML ResizeInfo where
  parseXML x =
    ResizeInfo'
      Lude.<$> (x Lude..@? "AllowCancelResize") Lude.<*> (x Lude..@? "ResizeType")
