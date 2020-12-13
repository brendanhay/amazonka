{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.Failure
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.Failure
  ( Failure (..),

    -- * Smart constructor
    mkFailure,

    -- * Lenses
    fArn,
    fReason,
    fDetail,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A failed resource. For a list of common causes, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/api_failures_messages.html API failure reasons> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /See:/ 'mkFailure' smart constructor.
data Failure = Failure'
  { -- | The Amazon Resource Name (ARN) of the failed resource.
    arn :: Lude.Maybe Lude.Text,
    -- | The reason for the failure.
    reason :: Lude.Maybe Lude.Text,
    -- | The details of the failure.
    detail :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Failure' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the failed resource.
-- * 'reason' - The reason for the failure.
-- * 'detail' - The details of the failure.
mkFailure ::
  Failure
mkFailure =
  Failure'
    { arn = Lude.Nothing,
      reason = Lude.Nothing,
      detail = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the failed resource.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fArn :: Lens.Lens' Failure (Lude.Maybe Lude.Text)
fArn = Lens.lens (arn :: Failure -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Failure)
{-# DEPRECATED fArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The reason for the failure.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fReason :: Lens.Lens' Failure (Lude.Maybe Lude.Text)
fReason = Lens.lens (reason :: Failure -> Lude.Maybe Lude.Text) (\s a -> s {reason = a} :: Failure)
{-# DEPRECATED fReason "Use generic-lens or generic-optics with 'reason' instead." #-}

-- | The details of the failure.
--
-- /Note:/ Consider using 'detail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fDetail :: Lens.Lens' Failure (Lude.Maybe Lude.Text)
fDetail = Lens.lens (detail :: Failure -> Lude.Maybe Lude.Text) (\s a -> s {detail = a} :: Failure)
{-# DEPRECATED fDetail "Use generic-lens or generic-optics with 'detail' instead." #-}

instance Lude.FromJSON Failure where
  parseJSON =
    Lude.withObject
      "Failure"
      ( \x ->
          Failure'
            Lude.<$> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "reason")
            Lude.<*> (x Lude..:? "detail")
      )
