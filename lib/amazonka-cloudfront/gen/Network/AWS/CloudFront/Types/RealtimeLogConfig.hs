{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.RealtimeLogConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.RealtimeLogConfig
  ( RealtimeLogConfig (..),

    -- * Smart constructor
    mkRealtimeLogConfig,

    -- * Lenses
    rlcARN,
    rlcName,
    rlcSamplingRate,
    rlcEndPoints,
    rlcFields,
  )
where

import qualified Network.AWS.CloudFront.Types.EndPoint as Types
import qualified Network.AWS.CloudFront.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A real-time log configuration.
--
-- /See:/ 'mkRealtimeLogConfig' smart constructor.
data RealtimeLogConfig = RealtimeLogConfig'
  { -- | The Amazon Resource Name (ARN) of this real-time log configuration.
    arn :: Types.String,
    -- | The unique name of this real-time log configuration.
    name :: Types.String,
    -- | The sampling rate for this real-time log configuration. The sampling rate determines the percentage of viewer requests that are represented in the real-time log data. The sampling rate is an integer between 1 and 100, inclusive.
    samplingRate :: Core.Integer,
    -- | Contains information about the Amazon Kinesis data stream where you are sending real-time log data for this real-time log configuration.
    endPoints :: [Types.EndPoint],
    -- | A list of fields that are included in each real-time log record. In an API response, the fields are provided in the same order in which they are sent to the Amazon Kinesis data stream.
    --
    -- For more information about fields, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html#understand-real-time-log-config-fields Real-time log configuration fields> in the /Amazon CloudFront Developer Guide/ .
    fields :: [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RealtimeLogConfig' value with any optional fields omitted.
mkRealtimeLogConfig ::
  -- | 'arn'
  Types.String ->
  -- | 'name'
  Types.String ->
  -- | 'samplingRate'
  Core.Integer ->
  RealtimeLogConfig
mkRealtimeLogConfig arn name samplingRate =
  RealtimeLogConfig'
    { arn,
      name,
      samplingRate,
      endPoints = Core.mempty,
      fields = Core.mempty
    }

-- | The Amazon Resource Name (ARN) of this real-time log configuration.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlcARN :: Lens.Lens' RealtimeLogConfig Types.String
rlcARN = Lens.field @"arn"
{-# DEPRECATED rlcARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The unique name of this real-time log configuration.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlcName :: Lens.Lens' RealtimeLogConfig Types.String
rlcName = Lens.field @"name"
{-# DEPRECATED rlcName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The sampling rate for this real-time log configuration. The sampling rate determines the percentage of viewer requests that are represented in the real-time log data. The sampling rate is an integer between 1 and 100, inclusive.
--
-- /Note:/ Consider using 'samplingRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlcSamplingRate :: Lens.Lens' RealtimeLogConfig Core.Integer
rlcSamplingRate = Lens.field @"samplingRate"
{-# DEPRECATED rlcSamplingRate "Use generic-lens or generic-optics with 'samplingRate' instead." #-}

-- | Contains information about the Amazon Kinesis data stream where you are sending real-time log data for this real-time log configuration.
--
-- /Note:/ Consider using 'endPoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlcEndPoints :: Lens.Lens' RealtimeLogConfig [Types.EndPoint]
rlcEndPoints = Lens.field @"endPoints"
{-# DEPRECATED rlcEndPoints "Use generic-lens or generic-optics with 'endPoints' instead." #-}

-- | A list of fields that are included in each real-time log record. In an API response, the fields are provided in the same order in which they are sent to the Amazon Kinesis data stream.
--
-- For more information about fields, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html#understand-real-time-log-config-fields Real-time log configuration fields> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'fields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlcFields :: Lens.Lens' RealtimeLogConfig [Types.String]
rlcFields = Lens.field @"fields"
{-# DEPRECATED rlcFields "Use generic-lens or generic-optics with 'fields' instead." #-}

instance Core.FromXML RealtimeLogConfig where
  parseXML x =
    RealtimeLogConfig'
      Core.<$> (x Core..@ "ARN")
      Core.<*> (x Core..@ "Name")
      Core.<*> (x Core..@ "SamplingRate")
      Core.<*> ( x Core..@? "EndPoints" Core..@! Core.mempty
                   Core..<@> Core.parseXMLList "member"
               )
      Core.<*> ( x Core..@? "Fields" Core..@! Core.mempty
                   Core..<@> Core.parseXMLList "Field"
               )
