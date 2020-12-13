{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.DeadLetterConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.DeadLetterConfig
  ( DeadLetterConfig (..),

    -- * Smart constructor
    mkDeadLetterConfig,

    -- * Lenses
    dlcARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A @DeadLetterConfig@ object that contains information about a dead-letter queue configuration.
--
-- /See:/ 'mkDeadLetterConfig' smart constructor.
newtype DeadLetterConfig = DeadLetterConfig'
  { -- | The ARN of the SQS queue specified as the target for the dead-letter queue.
    arn :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeadLetterConfig' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the SQS queue specified as the target for the dead-letter queue.
mkDeadLetterConfig ::
  DeadLetterConfig
mkDeadLetterConfig = DeadLetterConfig' {arn = Lude.Nothing}

-- | The ARN of the SQS queue specified as the target for the dead-letter queue.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlcARN :: Lens.Lens' DeadLetterConfig (Lude.Maybe Lude.Text)
dlcARN = Lens.lens (arn :: DeadLetterConfig -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: DeadLetterConfig)
{-# DEPRECATED dlcARN "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Lude.FromJSON DeadLetterConfig where
  parseJSON =
    Lude.withObject
      "DeadLetterConfig"
      (\x -> DeadLetterConfig' Lude.<$> (x Lude..:? "Arn"))

instance Lude.ToJSON DeadLetterConfig where
  toJSON DeadLetterConfig' {..} =
    Lude.object (Lude.catMaybes [("Arn" Lude..=) Lude.<$> arn])
