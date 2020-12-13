{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.DeadLetterConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.DeadLetterConfig
  ( DeadLetterConfig (..),

    -- * Smart constructor
    mkDeadLetterConfig,

    -- * Lenses
    dlcTargetARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The <https://docs.aws.amazon.com/lambda/latest/dg/invocation-async.html#dlq dead-letter queue> for failed asynchronous invocations.
--
-- /See:/ 'mkDeadLetterConfig' smart constructor.
newtype DeadLetterConfig = DeadLetterConfig'
  { -- | The Amazon Resource Name (ARN) of an Amazon SQS queue or Amazon SNS topic.
    targetARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeadLetterConfig' with the minimum fields required to make a request.
--
-- * 'targetARN' - The Amazon Resource Name (ARN) of an Amazon SQS queue or Amazon SNS topic.
mkDeadLetterConfig ::
  DeadLetterConfig
mkDeadLetterConfig = DeadLetterConfig' {targetARN = Lude.Nothing}

-- | The Amazon Resource Name (ARN) of an Amazon SQS queue or Amazon SNS topic.
--
-- /Note:/ Consider using 'targetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlcTargetARN :: Lens.Lens' DeadLetterConfig (Lude.Maybe Lude.Text)
dlcTargetARN = Lens.lens (targetARN :: DeadLetterConfig -> Lude.Maybe Lude.Text) (\s a -> s {targetARN = a} :: DeadLetterConfig)
{-# DEPRECATED dlcTargetARN "Use generic-lens or generic-optics with 'targetARN' instead." #-}

instance Lude.FromJSON DeadLetterConfig where
  parseJSON =
    Lude.withObject
      "DeadLetterConfig"
      (\x -> DeadLetterConfig' Lude.<$> (x Lude..:? "TargetArn"))

instance Lude.ToJSON DeadLetterConfig where
  toJSON DeadLetterConfig' {..} =
    Lude.object
      (Lude.catMaybes [("TargetArn" Lude..=) Lude.<$> targetARN])
