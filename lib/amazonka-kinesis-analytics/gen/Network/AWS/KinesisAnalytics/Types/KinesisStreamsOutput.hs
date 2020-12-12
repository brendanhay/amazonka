{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.KinesisStreamsOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.KinesisStreamsOutput
  ( KinesisStreamsOutput (..),

    -- * Smart constructor
    mkKinesisStreamsOutput,

    -- * Lenses
    ksoResourceARN,
    ksoRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | When configuring application output, identifies an Amazon Kinesis stream as the destination. You provide the stream Amazon Resource Name (ARN) and also an IAM role ARN that Amazon Kinesis Analytics can use to write to the stream on your behalf.
--
-- /See:/ 'mkKinesisStreamsOutput' smart constructor.
data KinesisStreamsOutput = KinesisStreamsOutput'
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

-- | Creates a value of 'KinesisStreamsOutput' with the minimum fields required to make a request.
--
-- * 'resourceARN' - ARN of the destination Amazon Kinesis stream to write to.
-- * 'roleARN' - ARN of the IAM role that Amazon Kinesis Analytics can assume to write to the destination stream on your behalf. You need to grant the necessary permissions to this role.
mkKinesisStreamsOutput ::
  -- | 'resourceARN'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  KinesisStreamsOutput
mkKinesisStreamsOutput pResourceARN_ pRoleARN_ =
  KinesisStreamsOutput'
    { resourceARN = pResourceARN_,
      roleARN = pRoleARN_
    }

-- | ARN of the destination Amazon Kinesis stream to write to.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ksoResourceARN :: Lens.Lens' KinesisStreamsOutput Lude.Text
ksoResourceARN = Lens.lens (resourceARN :: KinesisStreamsOutput -> Lude.Text) (\s a -> s {resourceARN = a} :: KinesisStreamsOutput)
{-# DEPRECATED ksoResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to write to the destination stream on your behalf. You need to grant the necessary permissions to this role.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ksoRoleARN :: Lens.Lens' KinesisStreamsOutput Lude.Text
ksoRoleARN = Lens.lens (roleARN :: KinesisStreamsOutput -> Lude.Text) (\s a -> s {roleARN = a} :: KinesisStreamsOutput)
{-# DEPRECATED ksoRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.ToJSON KinesisStreamsOutput where
  toJSON KinesisStreamsOutput' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ResourceARN" Lude..= resourceARN),
            Lude.Just ("RoleARN" Lude..= roleARN)
          ]
      )
