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

import qualified Network.AWS.KinesisAnalytics.Types.ResourceARN as Types
import qualified Network.AWS.KinesisAnalytics.Types.RoleARN as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | When configuring application output, identifies an Amazon Kinesis stream as the destination. You provide the stream Amazon Resource Name (ARN) and also an IAM role ARN that Amazon Kinesis Analytics can use to write to the stream on your behalf.
--
-- /See:/ 'mkKinesisStreamsOutput' smart constructor.
data KinesisStreamsOutput = KinesisStreamsOutput'
  { -- | ARN of the destination Amazon Kinesis stream to write to.
    resourceARN :: Types.ResourceARN,
    -- | ARN of the IAM role that Amazon Kinesis Analytics can assume to write to the destination stream on your behalf. You need to grant the necessary permissions to this role.
    roleARN :: Types.RoleARN
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'KinesisStreamsOutput' value with any optional fields omitted.
mkKinesisStreamsOutput ::
  -- | 'resourceARN'
  Types.ResourceARN ->
  -- | 'roleARN'
  Types.RoleARN ->
  KinesisStreamsOutput
mkKinesisStreamsOutput resourceARN roleARN =
  KinesisStreamsOutput' {resourceARN, roleARN}

-- | ARN of the destination Amazon Kinesis stream to write to.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ksoResourceARN :: Lens.Lens' KinesisStreamsOutput Types.ResourceARN
ksoResourceARN = Lens.field @"resourceARN"
{-# DEPRECATED ksoResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to write to the destination stream on your behalf. You need to grant the necessary permissions to this role.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ksoRoleARN :: Lens.Lens' KinesisStreamsOutput Types.RoleARN
ksoRoleARN = Lens.field @"roleARN"
{-# DEPRECATED ksoRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Core.FromJSON KinesisStreamsOutput where
  toJSON KinesisStreamsOutput {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ResourceARN" Core..= resourceARN),
            Core.Just ("RoleARN" Core..= roleARN)
          ]
      )
