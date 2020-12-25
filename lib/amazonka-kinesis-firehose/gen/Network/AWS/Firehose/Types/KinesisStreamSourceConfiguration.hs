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

import qualified Network.AWS.Firehose.Types.KinesisStreamARN as Types
import qualified Network.AWS.Firehose.Types.RoleARN as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The stream and role Amazon Resource Names (ARNs) for a Kinesis data stream used as the source for a delivery stream.
--
-- /See:/ 'mkKinesisStreamSourceConfiguration' smart constructor.
data KinesisStreamSourceConfiguration = KinesisStreamSourceConfiguration'
  { -- | The ARN of the source Kinesis data stream. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kinesis-streams Amazon Kinesis Data Streams ARN Format> .
    kinesisStreamARN :: Types.KinesisStreamARN,
    -- | The ARN of the role that provides access to the source Kinesis data stream. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam AWS Identity and Access Management (IAM) ARN Format> .
    roleARN :: Types.RoleARN
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'KinesisStreamSourceConfiguration' value with any optional fields omitted.
mkKinesisStreamSourceConfiguration ::
  -- | 'kinesisStreamARN'
  Types.KinesisStreamARN ->
  -- | 'roleARN'
  Types.RoleARN ->
  KinesisStreamSourceConfiguration
mkKinesisStreamSourceConfiguration kinesisStreamARN roleARN =
  KinesisStreamSourceConfiguration' {kinesisStreamARN, roleARN}

-- | The ARN of the source Kinesis data stream. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kinesis-streams Amazon Kinesis Data Streams ARN Format> .
--
-- /Note:/ Consider using 'kinesisStreamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ksscKinesisStreamARN :: Lens.Lens' KinesisStreamSourceConfiguration Types.KinesisStreamARN
ksscKinesisStreamARN = Lens.field @"kinesisStreamARN"
{-# DEPRECATED ksscKinesisStreamARN "Use generic-lens or generic-optics with 'kinesisStreamARN' instead." #-}

-- | The ARN of the role that provides access to the source Kinesis data stream. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam AWS Identity and Access Management (IAM) ARN Format> .
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ksscRoleARN :: Lens.Lens' KinesisStreamSourceConfiguration Types.RoleARN
ksscRoleARN = Lens.field @"roleARN"
{-# DEPRECATED ksscRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Core.FromJSON KinesisStreamSourceConfiguration where
  toJSON KinesisStreamSourceConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("KinesisStreamARN" Core..= kinesisStreamARN),
            Core.Just ("RoleARN" Core..= roleARN)
          ]
      )
