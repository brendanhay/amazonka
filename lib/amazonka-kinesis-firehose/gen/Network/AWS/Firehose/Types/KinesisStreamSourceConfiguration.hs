{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.KinesisStreamSourceConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.KinesisStreamSourceConfiguration
  ( KinesisStreamSourceConfiguration (..),

    -- * Smart constructor
    mkKinesisStreamSourceConfiguration,

    -- * Lenses
    ksscKinesisStreamARN,
    ksscRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The stream and role Amazon Resource Names (ARNs) for a Kinesis data stream used as the source for a delivery stream.
--
-- /See:/ 'mkKinesisStreamSourceConfiguration' smart constructor.
data KinesisStreamSourceConfiguration = KinesisStreamSourceConfiguration'
  { kinesisStreamARN ::
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

-- | Creates a value of 'KinesisStreamSourceConfiguration' with the minimum fields required to make a request.
--
-- * 'kinesisStreamARN' - The ARN of the source Kinesis data stream. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kinesis-streams Amazon Kinesis Data Streams ARN Format> .
-- * 'roleARN' - The ARN of the role that provides access to the source Kinesis data stream. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam AWS Identity and Access Management (IAM) ARN Format> .
mkKinesisStreamSourceConfiguration ::
  -- | 'kinesisStreamARN'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  KinesisStreamSourceConfiguration
mkKinesisStreamSourceConfiguration pKinesisStreamARN_ pRoleARN_ =
  KinesisStreamSourceConfiguration'
    { kinesisStreamARN =
        pKinesisStreamARN_,
      roleARN = pRoleARN_
    }

-- | The ARN of the source Kinesis data stream. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kinesis-streams Amazon Kinesis Data Streams ARN Format> .
--
-- /Note:/ Consider using 'kinesisStreamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ksscKinesisStreamARN :: Lens.Lens' KinesisStreamSourceConfiguration Lude.Text
ksscKinesisStreamARN = Lens.lens (kinesisStreamARN :: KinesisStreamSourceConfiguration -> Lude.Text) (\s a -> s {kinesisStreamARN = a} :: KinesisStreamSourceConfiguration)
{-# DEPRECATED ksscKinesisStreamARN "Use generic-lens or generic-optics with 'kinesisStreamARN' instead." #-}

-- | The ARN of the role that provides access to the source Kinesis data stream. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam AWS Identity and Access Management (IAM) ARN Format> .
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ksscRoleARN :: Lens.Lens' KinesisStreamSourceConfiguration Lude.Text
ksscRoleARN = Lens.lens (roleARN :: KinesisStreamSourceConfiguration -> Lude.Text) (\s a -> s {roleARN = a} :: KinesisStreamSourceConfiguration)
{-# DEPRECATED ksscRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.ToJSON KinesisStreamSourceConfiguration where
  toJSON KinesisStreamSourceConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("KinesisStreamARN" Lude..= kinesisStreamARN),
            Lude.Just ("RoleARN" Lude..= roleARN)
          ]
      )
