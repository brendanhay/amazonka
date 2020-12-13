{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.QueueReference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.QueueReference
  ( QueueReference (..),

    -- * Smart constructor
    mkQueueReference,

    -- * Lenses
    qrARN,
    qrId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about a queue resource for which metrics are returned.
--
-- /See:/ 'mkQueueReference' smart constructor.
data QueueReference = QueueReference'
  { -- | The Amazon Resource Name (ARN) of the queue.
    arn :: Lude.Maybe Lude.Text,
    -- | The identifier of the queue.
    id :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'QueueReference' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the queue.
-- * 'id' - The identifier of the queue.
mkQueueReference ::
  QueueReference
mkQueueReference =
  QueueReference' {arn = Lude.Nothing, id = Lude.Nothing}

-- | The Amazon Resource Name (ARN) of the queue.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qrARN :: Lens.Lens' QueueReference (Lude.Maybe Lude.Text)
qrARN = Lens.lens (arn :: QueueReference -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: QueueReference)
{-# DEPRECATED qrARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The identifier of the queue.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qrId :: Lens.Lens' QueueReference (Lude.Maybe Lude.Text)
qrId = Lens.lens (id :: QueueReference -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: QueueReference)
{-# DEPRECATED qrId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromJSON QueueReference where
  parseJSON =
    Lude.withObject
      "QueueReference"
      ( \x ->
          QueueReference'
            Lude.<$> (x Lude..:? "Arn") Lude.<*> (x Lude..:? "Id")
      )
