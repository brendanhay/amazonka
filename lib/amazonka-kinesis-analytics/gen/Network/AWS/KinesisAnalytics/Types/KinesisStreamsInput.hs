{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.KinesisStreamsInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.KinesisStreamsInput
  ( KinesisStreamsInput (..),

    -- * Smart constructor
    mkKinesisStreamsInput,

    -- * Lenses
    ksiResourceARN,
    ksiRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Identifies an Amazon Kinesis stream as the streaming source. You provide the stream's Amazon Resource Name (ARN) and an IAM role ARN that enables Amazon Kinesis Analytics to access the stream on your behalf.
--
-- /See:/ 'mkKinesisStreamsInput' smart constructor.
data KinesisStreamsInput = KinesisStreamsInput'
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

-- | Creates a value of 'KinesisStreamsInput' with the minimum fields required to make a request.
--
-- * 'resourceARN' - ARN of the input Amazon Kinesis stream to read.
-- * 'roleARN' - ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream on your behalf. You need to grant the necessary permissions to this role.
mkKinesisStreamsInput ::
  -- | 'resourceARN'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  KinesisStreamsInput
mkKinesisStreamsInput pResourceARN_ pRoleARN_ =
  KinesisStreamsInput'
    { resourceARN = pResourceARN_,
      roleARN = pRoleARN_
    }

-- | ARN of the input Amazon Kinesis stream to read.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ksiResourceARN :: Lens.Lens' KinesisStreamsInput Lude.Text
ksiResourceARN = Lens.lens (resourceARN :: KinesisStreamsInput -> Lude.Text) (\s a -> s {resourceARN = a} :: KinesisStreamsInput)
{-# DEPRECATED ksiResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream on your behalf. You need to grant the necessary permissions to this role.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ksiRoleARN :: Lens.Lens' KinesisStreamsInput Lude.Text
ksiRoleARN = Lens.lens (roleARN :: KinesisStreamsInput -> Lude.Text) (\s a -> s {roleARN = a} :: KinesisStreamsInput)
{-# DEPRECATED ksiRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.ToJSON KinesisStreamsInput where
  toJSON KinesisStreamsInput' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ResourceARN" Lude..= resourceARN),
            Lude.Just ("RoleARN" Lude..= roleARN)
          ]
      )
