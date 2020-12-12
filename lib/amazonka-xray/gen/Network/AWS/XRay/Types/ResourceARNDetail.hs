{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.ResourceARNDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.ResourceARNDetail
  ( ResourceARNDetail (..),

    -- * Smart constructor
    mkResourceARNDetail,

    -- * Lenses
    radARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A list of resources ARNs corresponding to the segments in a trace.
--
-- /See:/ 'mkResourceARNDetail' smart constructor.
newtype ResourceARNDetail = ResourceARNDetail'
  { arn ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceARNDetail' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of a corresponding resource.
mkResourceARNDetail ::
  ResourceARNDetail
mkResourceARNDetail = ResourceARNDetail' {arn = Lude.Nothing}

-- | The ARN of a corresponding resource.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
radARN :: Lens.Lens' ResourceARNDetail (Lude.Maybe Lude.Text)
radARN = Lens.lens (arn :: ResourceARNDetail -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: ResourceARNDetail)
{-# DEPRECATED radARN "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Lude.FromJSON ResourceARNDetail where
  parseJSON =
    Lude.withObject
      "ResourceARNDetail"
      (\x -> ResourceARNDetail' Lude.<$> (x Lude..:? "ARN"))
