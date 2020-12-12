{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.KinesisFirehoseInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.KinesisFirehoseInput
  ( KinesisFirehoseInput (..),

    -- * Smart constructor
    mkKinesisFirehoseInput,

    -- * Lenses
    kfiResourceARN,
    kfiRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Identifies an Amazon Kinesis Firehose delivery stream as the streaming source. You provide the delivery stream's Amazon Resource Name (ARN) and an IAM role ARN that enables Amazon Kinesis Analytics to access the stream on your behalf.
--
-- /See:/ 'mkKinesisFirehoseInput' smart constructor.
data KinesisFirehoseInput = KinesisFirehoseInput'
  { resourceARN ::
      Lude.Text,
    roleARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'KinesisFirehoseInput' with the minimum fields required to make a request.
--
-- * 'resourceARN' - ARN of the input delivery stream.
-- * 'roleARN' - ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream on your behalf. You need to make sure that the role has the necessary permissions to access the stream.
mkKinesisFirehoseInput ::
  -- | 'resourceARN'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  KinesisFirehoseInput
mkKinesisFirehoseInput pResourceARN_ pRoleARN_ =
  KinesisFirehoseInput'
    { resourceARN = pResourceARN_,
      roleARN = pRoleARN_
    }

-- | ARN of the input delivery stream.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kfiResourceARN :: Lens.Lens' KinesisFirehoseInput Lude.Text
kfiResourceARN = Lens.lens (resourceARN :: KinesisFirehoseInput -> Lude.Text) (\s a -> s {resourceARN = a} :: KinesisFirehoseInput)
{-# DEPRECATED kfiResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream on your behalf. You need to make sure that the role has the necessary permissions to access the stream.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kfiRoleARN :: Lens.Lens' KinesisFirehoseInput Lude.Text
kfiRoleARN = Lens.lens (roleARN :: KinesisFirehoseInput -> Lude.Text) (\s a -> s {roleARN = a} :: KinesisFirehoseInput)
{-# DEPRECATED kfiRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.ToJSON KinesisFirehoseInput where
  toJSON KinesisFirehoseInput' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ResourceARN" Lude..= resourceARN),
            Lude.Just ("RoleARN" Lude..= roleARN)
          ]
      )
