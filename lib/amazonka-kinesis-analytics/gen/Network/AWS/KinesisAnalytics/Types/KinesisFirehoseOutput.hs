{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.KinesisFirehoseOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.KinesisFirehoseOutput
  ( KinesisFirehoseOutput (..),

    -- * Smart constructor
    mkKinesisFirehoseOutput,

    -- * Lenses
    kfoResourceARN,
    kfoRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | When configuring application output, identifies an Amazon Kinesis Firehose delivery stream as the destination. You provide the stream Amazon Resource Name (ARN) and an IAM role that enables Amazon Kinesis Analytics to write to the stream on your behalf.
--
-- /See:/ 'mkKinesisFirehoseOutput' smart constructor.
data KinesisFirehoseOutput = KinesisFirehoseOutput'
  { -- | ARN of the destination Amazon Kinesis Firehose delivery stream to write to.
    resourceARN :: Lude.Text,
    -- | ARN of the IAM role that Amazon Kinesis Analytics can assume to write to the destination stream on your behalf. You need to grant the necessary permissions to this role.
    roleARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'KinesisFirehoseOutput' with the minimum fields required to make a request.
--
-- * 'resourceARN' - ARN of the destination Amazon Kinesis Firehose delivery stream to write to.
-- * 'roleARN' - ARN of the IAM role that Amazon Kinesis Analytics can assume to write to the destination stream on your behalf. You need to grant the necessary permissions to this role.
mkKinesisFirehoseOutput ::
  -- | 'resourceARN'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  KinesisFirehoseOutput
mkKinesisFirehoseOutput pResourceARN_ pRoleARN_ =
  KinesisFirehoseOutput'
    { resourceARN = pResourceARN_,
      roleARN = pRoleARN_
    }

-- | ARN of the destination Amazon Kinesis Firehose delivery stream to write to.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kfoResourceARN :: Lens.Lens' KinesisFirehoseOutput Lude.Text
kfoResourceARN = Lens.lens (resourceARN :: KinesisFirehoseOutput -> Lude.Text) (\s a -> s {resourceARN = a} :: KinesisFirehoseOutput)
{-# DEPRECATED kfoResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to write to the destination stream on your behalf. You need to grant the necessary permissions to this role.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kfoRoleARN :: Lens.Lens' KinesisFirehoseOutput Lude.Text
kfoRoleARN = Lens.lens (roleARN :: KinesisFirehoseOutput -> Lude.Text) (\s a -> s {roleARN = a} :: KinesisFirehoseOutput)
{-# DEPRECATED kfoRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.ToJSON KinesisFirehoseOutput where
  toJSON KinesisFirehoseOutput' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ResourceARN" Lude..= resourceARN),
            Lude.Just ("RoleARN" Lude..= roleARN)
          ]
      )
