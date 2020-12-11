-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.InstanceIdDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.InstanceIdDetail
  ( InstanceIdDetail (..),

    -- * Smart constructor
    mkInstanceIdDetail,

    -- * Lenses
    iidId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A list of EC2 instance IDs corresponding to the segments in a trace.
--
-- /See:/ 'mkInstanceIdDetail' smart constructor.
newtype InstanceIdDetail = InstanceIdDetail'
  { id ::
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

-- | Creates a value of 'InstanceIdDetail' with the minimum fields required to make a request.
--
-- * 'id' - The ID of a corresponding EC2 instance.
mkInstanceIdDetail ::
  InstanceIdDetail
mkInstanceIdDetail = InstanceIdDetail' {id = Lude.Nothing}

-- | The ID of a corresponding EC2 instance.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iidId :: Lens.Lens' InstanceIdDetail (Lude.Maybe Lude.Text)
iidId = Lens.lens (id :: InstanceIdDetail -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: InstanceIdDetail)
{-# DEPRECATED iidId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromJSON InstanceIdDetail where
  parseJSON =
    Lude.withObject
      "InstanceIdDetail"
      (\x -> InstanceIdDetail' Lude.<$> (x Lude..:? "Id"))
