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

import Network.AWS.CloudFront.Types.EndPoint
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A real-time log configuration.
--
-- /See:/ 'mkRealtimeLogConfig' smart constructor.
data RealtimeLogConfig = RealtimeLogConfig'
  { arn :: Lude.Text,
    name :: Lude.Text,
    samplingRate :: Lude.Integer,
    endPoints :: [EndPoint],
    fields :: [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RealtimeLogConfig' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of this real-time log configuration.
-- * 'endPoints' - Contains information about the Amazon Kinesis data stream where you are sending real-time log data for this real-time log configuration.
-- * 'fields' - A list of fields that are included in each real-time log record. In an API response, the fields are provided in the same order in which they are sent to the Amazon Kinesis data stream.
--
-- For more information about fields, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html#understand-real-time-log-config-fields Real-time log configuration fields> in the /Amazon CloudFront Developer Guide/ .
-- * 'name' - The unique name of this real-time log configuration.
-- * 'samplingRate' - The sampling rate for this real-time log configuration. The sampling rate determines the percentage of viewer requests that are represented in the real-time log data. The sampling rate is an integer between 1 and 100, inclusive.
mkRealtimeLogConfig ::
  -- | 'arn'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  -- | 'samplingRate'
  Lude.Integer ->
  RealtimeLogConfig
mkRealtimeLogConfig pARN_ pName_ pSamplingRate_ =
  RealtimeLogConfig'
    { arn = pARN_,
      name = pName_,
      samplingRate = pSamplingRate_,
      endPoints = Lude.mempty,
      fields = Lude.mempty
    }

-- | The Amazon Resource Name (ARN) of this real-time log configuration.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlcARN :: Lens.Lens' RealtimeLogConfig Lude.Text
rlcARN = Lens.lens (arn :: RealtimeLogConfig -> Lude.Text) (\s a -> s {arn = a} :: RealtimeLogConfig)
{-# DEPRECATED rlcARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The unique name of this real-time log configuration.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlcName :: Lens.Lens' RealtimeLogConfig Lude.Text
rlcName = Lens.lens (name :: RealtimeLogConfig -> Lude.Text) (\s a -> s {name = a} :: RealtimeLogConfig)
{-# DEPRECATED rlcName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The sampling rate for this real-time log configuration. The sampling rate determines the percentage of viewer requests that are represented in the real-time log data. The sampling rate is an integer between 1 and 100, inclusive.
--
-- /Note:/ Consider using 'samplingRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlcSamplingRate :: Lens.Lens' RealtimeLogConfig Lude.Integer
rlcSamplingRate = Lens.lens (samplingRate :: RealtimeLogConfig -> Lude.Integer) (\s a -> s {samplingRate = a} :: RealtimeLogConfig)
{-# DEPRECATED rlcSamplingRate "Use generic-lens or generic-optics with 'samplingRate' instead." #-}

-- | Contains information about the Amazon Kinesis data stream where you are sending real-time log data for this real-time log configuration.
--
-- /Note:/ Consider using 'endPoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlcEndPoints :: Lens.Lens' RealtimeLogConfig [EndPoint]
rlcEndPoints = Lens.lens (endPoints :: RealtimeLogConfig -> [EndPoint]) (\s a -> s {endPoints = a} :: RealtimeLogConfig)
{-# DEPRECATED rlcEndPoints "Use generic-lens or generic-optics with 'endPoints' instead." #-}

-- | A list of fields that are included in each real-time log record. In an API response, the fields are provided in the same order in which they are sent to the Amazon Kinesis data stream.
--
-- For more information about fields, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html#understand-real-time-log-config-fields Real-time log configuration fields> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'fields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlcFields :: Lens.Lens' RealtimeLogConfig [Lude.Text]
rlcFields = Lens.lens (fields :: RealtimeLogConfig -> [Lude.Text]) (\s a -> s {fields = a} :: RealtimeLogConfig)
{-# DEPRECATED rlcFields "Use generic-lens or generic-optics with 'fields' instead." #-}

instance Lude.FromXML RealtimeLogConfig where
  parseXML x =
    RealtimeLogConfig'
      Lude.<$> (x Lude..@ "ARN")
      Lude.<*> (x Lude..@ "Name")
      Lude.<*> (x Lude..@ "SamplingRate")
      Lude.<*> ( x Lude..@? "EndPoints" Lude..!@ Lude.mempty
                   Lude.>>= Lude.parseXMLList "member"
               )
      Lude.<*> ( x Lude..@? "Fields" Lude..!@ Lude.mempty
                   Lude.>>= Lude.parseXMLList "Field"
               )
