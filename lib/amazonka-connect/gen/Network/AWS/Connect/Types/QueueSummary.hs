{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.QueueSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.QueueSummary
  ( QueueSummary (..),

    -- * Smart constructor
    mkQueueSummary,

    -- * Lenses
    qsARN,
    qsName,
    qsId,
    qsQueueType,
  )
where

import Network.AWS.Connect.Types.QueueType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains summary information about a queue.
--
-- /See:/ 'mkQueueSummary' smart constructor.
data QueueSummary = QueueSummary'
  { arn :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text,
    queueType :: Lude.Maybe QueueType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'QueueSummary' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the queue.
-- * 'id' - The identifier of the queue.
-- * 'name' - The name of the queue.
-- * 'queueType' - The type of queue.
mkQueueSummary ::
  QueueSummary
mkQueueSummary =
  QueueSummary'
    { arn = Lude.Nothing,
      name = Lude.Nothing,
      id = Lude.Nothing,
      queueType = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the queue.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsARN :: Lens.Lens' QueueSummary (Lude.Maybe Lude.Text)
qsARN = Lens.lens (arn :: QueueSummary -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: QueueSummary)
{-# DEPRECATED qsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the queue.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsName :: Lens.Lens' QueueSummary (Lude.Maybe Lude.Text)
qsName = Lens.lens (name :: QueueSummary -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: QueueSummary)
{-# DEPRECATED qsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The identifier of the queue.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsId :: Lens.Lens' QueueSummary (Lude.Maybe Lude.Text)
qsId = Lens.lens (id :: QueueSummary -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: QueueSummary)
{-# DEPRECATED qsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The type of queue.
--
-- /Note:/ Consider using 'queueType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsQueueType :: Lens.Lens' QueueSummary (Lude.Maybe QueueType)
qsQueueType = Lens.lens (queueType :: QueueSummary -> Lude.Maybe QueueType) (\s a -> s {queueType = a} :: QueueSummary)
{-# DEPRECATED qsQueueType "Use generic-lens or generic-optics with 'queueType' instead." #-}

instance Lude.FromJSON QueueSummary where
  parseJSON =
    Lude.withObject
      "QueueSummary"
      ( \x ->
          QueueSummary'
            Lude.<$> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "QueueType")
      )
