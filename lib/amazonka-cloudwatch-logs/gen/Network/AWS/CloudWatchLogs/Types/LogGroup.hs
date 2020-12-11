-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.LogGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types.LogGroup
  ( LogGroup (..),

    -- * Smart constructor
    mkLogGroup,

    -- * Lenses
    lgCreationTime,
    lgMetricFilterCount,
    lgArn,
    lgLogGroupName,
    lgRetentionInDays,
    lgKmsKeyId,
    lgStoredBytes,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a log group.
--
-- /See:/ 'mkLogGroup' smart constructor.
data LogGroup = LogGroup'
  { creationTime :: Lude.Maybe Lude.Natural,
    metricFilterCount :: Lude.Maybe Lude.Int,
    arn :: Lude.Maybe Lude.Text,
    logGroupName :: Lude.Maybe Lude.Text,
    retentionInDays :: Lude.Maybe Lude.Int,
    kmsKeyId :: Lude.Maybe Lude.Text,
    storedBytes :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LogGroup' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the log group.
-- * 'creationTime' - The creation time of the log group, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
-- * 'kmsKeyId' - The Amazon Resource Name (ARN) of the CMK to use when encrypting log data.
-- * 'logGroupName' - The name of the log group.
-- * 'metricFilterCount' - The number of metric filters.
-- * 'retentionInDays' - Undocumented field.
-- * 'storedBytes' - The number of bytes stored.
mkLogGroup ::
  LogGroup
mkLogGroup =
  LogGroup'
    { creationTime = Lude.Nothing,
      metricFilterCount = Lude.Nothing,
      arn = Lude.Nothing,
      logGroupName = Lude.Nothing,
      retentionInDays = Lude.Nothing,
      kmsKeyId = Lude.Nothing,
      storedBytes = Lude.Nothing
    }

-- | The creation time of the log group, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgCreationTime :: Lens.Lens' LogGroup (Lude.Maybe Lude.Natural)
lgCreationTime = Lens.lens (creationTime :: LogGroup -> Lude.Maybe Lude.Natural) (\s a -> s {creationTime = a} :: LogGroup)
{-# DEPRECATED lgCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The number of metric filters.
--
-- /Note:/ Consider using 'metricFilterCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgMetricFilterCount :: Lens.Lens' LogGroup (Lude.Maybe Lude.Int)
lgMetricFilterCount = Lens.lens (metricFilterCount :: LogGroup -> Lude.Maybe Lude.Int) (\s a -> s {metricFilterCount = a} :: LogGroup)
{-# DEPRECATED lgMetricFilterCount "Use generic-lens or generic-optics with 'metricFilterCount' instead." #-}

-- | The Amazon Resource Name (ARN) of the log group.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgArn :: Lens.Lens' LogGroup (Lude.Maybe Lude.Text)
lgArn = Lens.lens (arn :: LogGroup -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: LogGroup)
{-# DEPRECATED lgArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the log group.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgLogGroupName :: Lens.Lens' LogGroup (Lude.Maybe Lude.Text)
lgLogGroupName = Lens.lens (logGroupName :: LogGroup -> Lude.Maybe Lude.Text) (\s a -> s {logGroupName = a} :: LogGroup)
{-# DEPRECATED lgLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'retentionInDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgRetentionInDays :: Lens.Lens' LogGroup (Lude.Maybe Lude.Int)
lgRetentionInDays = Lens.lens (retentionInDays :: LogGroup -> Lude.Maybe Lude.Int) (\s a -> s {retentionInDays = a} :: LogGroup)
{-# DEPRECATED lgRetentionInDays "Use generic-lens or generic-optics with 'retentionInDays' instead." #-}

-- | The Amazon Resource Name (ARN) of the CMK to use when encrypting log data.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgKmsKeyId :: Lens.Lens' LogGroup (Lude.Maybe Lude.Text)
lgKmsKeyId = Lens.lens (kmsKeyId :: LogGroup -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: LogGroup)
{-# DEPRECATED lgKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The number of bytes stored.
--
-- /Note:/ Consider using 'storedBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgStoredBytes :: Lens.Lens' LogGroup (Lude.Maybe Lude.Natural)
lgStoredBytes = Lens.lens (storedBytes :: LogGroup -> Lude.Maybe Lude.Natural) (\s a -> s {storedBytes = a} :: LogGroup)
{-# DEPRECATED lgStoredBytes "Use generic-lens or generic-optics with 'storedBytes' instead." #-}

instance Lude.FromJSON LogGroup where
  parseJSON =
    Lude.withObject
      "LogGroup"
      ( \x ->
          LogGroup'
            Lude.<$> (x Lude..:? "creationTime")
            Lude.<*> (x Lude..:? "metricFilterCount")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "logGroupName")
            Lude.<*> (x Lude..:? "retentionInDays")
            Lude.<*> (x Lude..:? "kmsKeyId")
            Lude.<*> (x Lude..:? "storedBytes")
      )
