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

import qualified Network.AWS.KinesisAnalytics.Types.ResourceARN as Types
import qualified Network.AWS.KinesisAnalytics.Types.RoleARN as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | When configuring application output, identifies an Amazon Kinesis Firehose delivery stream as the destination. You provide the stream Amazon Resource Name (ARN) and an IAM role that enables Amazon Kinesis Analytics to write to the stream on your behalf.
--
-- /See:/ 'mkKinesisFirehoseOutput' smart constructor.
data KinesisFirehoseOutput = KinesisFirehoseOutput'
  { -- | ARN of the destination Amazon Kinesis Firehose delivery stream to write to.
    resourceARN :: Types.ResourceARN,
    -- | ARN of the IAM role that Amazon Kinesis Analytics can assume to write to the destination stream on your behalf. You need to grant the necessary permissions to this role.
    roleARN :: Types.RoleARN
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'KinesisFirehoseOutput' value with any optional fields omitted.
mkKinesisFirehoseOutput ::
  -- | 'resourceARN'
  Types.ResourceARN ->
  -- | 'roleARN'
  Types.RoleARN ->
  KinesisFirehoseOutput
mkKinesisFirehoseOutput resourceARN roleARN =
  KinesisFirehoseOutput' {resourceARN, roleARN}

-- | ARN of the destination Amazon Kinesis Firehose delivery stream to write to.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kfoResourceARN :: Lens.Lens' KinesisFirehoseOutput Types.ResourceARN
kfoResourceARN = Lens.field @"resourceARN"
{-# DEPRECATED kfoResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to write to the destination stream on your behalf. You need to grant the necessary permissions to this role.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kfoRoleARN :: Lens.Lens' KinesisFirehoseOutput Types.RoleARN
kfoRoleARN = Lens.field @"roleARN"
{-# DEPRECATED kfoRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Core.FromJSON KinesisFirehoseOutput where
  toJSON KinesisFirehoseOutput {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ResourceARN" Core..= resourceARN),
            Core.Just ("RoleARN" Core..= roleARN)
          ]
      )
