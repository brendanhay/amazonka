{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.SqsParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.SqsParameters
  ( SqsParameters (..),

    -- * Smart constructor
    mkSqsParameters,

    -- * Lenses
    spMessageGroupId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | This structure includes the custom parameter to be used when the target is an SQS FIFO queue.
--
-- /See:/ 'mkSqsParameters' smart constructor.
newtype SqsParameters = SqsParameters'
  { -- | The FIFO message group ID to use as the target.
    messageGroupId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SqsParameters' with the minimum fields required to make a request.
--
-- * 'messageGroupId' - The FIFO message group ID to use as the target.
mkSqsParameters ::
  SqsParameters
mkSqsParameters = SqsParameters' {messageGroupId = Lude.Nothing}

-- | The FIFO message group ID to use as the target.
--
-- /Note:/ Consider using 'messageGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spMessageGroupId :: Lens.Lens' SqsParameters (Lude.Maybe Lude.Text)
spMessageGroupId = Lens.lens (messageGroupId :: SqsParameters -> Lude.Maybe Lude.Text) (\s a -> s {messageGroupId = a} :: SqsParameters)
{-# DEPRECATED spMessageGroupId "Use generic-lens or generic-optics with 'messageGroupId' instead." #-}

instance Lude.FromJSON SqsParameters where
  parseJSON =
    Lude.withObject
      "SqsParameters"
      (\x -> SqsParameters' Lude.<$> (x Lude..:? "MessageGroupId"))

instance Lude.ToJSON SqsParameters where
  toJSON SqsParameters' {..} =
    Lude.object
      ( Lude.catMaybes
          [("MessageGroupId" Lude..=) Lude.<$> messageGroupId]
      )
