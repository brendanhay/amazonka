{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.KinesisFirehoseOutputDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.KinesisFirehoseOutputDescription
  ( KinesisFirehoseOutputDescription (..),

    -- * Smart constructor
    mkKinesisFirehoseOutputDescription,

    -- * Lenses
    kfodResourceARN,
    kfodRoleARN,
  )
where

import qualified Network.AWS.KinesisAnalytics.Types.ResourceARN as Types
import qualified Network.AWS.KinesisAnalytics.Types.RoleARN as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | For an application output, describes the Amazon Kinesis Firehose delivery stream configured as its destination.
--
-- /See:/ 'mkKinesisFirehoseOutputDescription' smart constructor.
data KinesisFirehoseOutputDescription = KinesisFirehoseOutputDescription'
  { -- | Amazon Resource Name (ARN) of the Amazon Kinesis Firehose delivery stream.
    resourceARN :: Core.Maybe Types.ResourceARN,
    -- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream.
    roleARN :: Core.Maybe Types.RoleARN
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'KinesisFirehoseOutputDescription' value with any optional fields omitted.
mkKinesisFirehoseOutputDescription ::
  KinesisFirehoseOutputDescription
mkKinesisFirehoseOutputDescription =
  KinesisFirehoseOutputDescription'
    { resourceARN = Core.Nothing,
      roleARN = Core.Nothing
    }

-- | Amazon Resource Name (ARN) of the Amazon Kinesis Firehose delivery stream.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kfodResourceARN :: Lens.Lens' KinesisFirehoseOutputDescription (Core.Maybe Types.ResourceARN)
kfodResourceARN = Lens.field @"resourceARN"
{-# DEPRECATED kfodResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kfodRoleARN :: Lens.Lens' KinesisFirehoseOutputDescription (Core.Maybe Types.RoleARN)
kfodRoleARN = Lens.field @"roleARN"
{-# DEPRECATED kfodRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Core.FromJSON KinesisFirehoseOutputDescription where
  parseJSON =
    Core.withObject "KinesisFirehoseOutputDescription" Core.$
      \x ->
        KinesisFirehoseOutputDescription'
          Core.<$> (x Core..:? "ResourceARN") Core.<*> (x Core..:? "RoleARN")
