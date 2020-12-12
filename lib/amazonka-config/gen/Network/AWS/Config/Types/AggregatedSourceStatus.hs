{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.AggregatedSourceStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.AggregatedSourceStatus
  ( AggregatedSourceStatus (..),

    -- * Smart constructor
    mkAggregatedSourceStatus,

    -- * Lenses
    assLastErrorCode,
    assLastUpdateStatus,
    assSourceType,
    assSourceId,
    assLastErrorMessage,
    assAWSRegion,
    assLastUpdateTime,
  )
where

import Network.AWS.Config.Types.AggregatedSourceStatusType
import Network.AWS.Config.Types.AggregatedSourceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The current sync status between the source and the aggregator account.
--
-- /See:/ 'mkAggregatedSourceStatus' smart constructor.
data AggregatedSourceStatus = AggregatedSourceStatus'
  { lastErrorCode ::
      Lude.Maybe Lude.Text,
    lastUpdateStatus ::
      Lude.Maybe AggregatedSourceStatusType,
    sourceType :: Lude.Maybe AggregatedSourceType,
    sourceId :: Lude.Maybe Lude.Text,
    lastErrorMessage :: Lude.Maybe Lude.Text,
    awsRegion :: Lude.Maybe Lude.Text,
    lastUpdateTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AggregatedSourceStatus' with the minimum fields required to make a request.
--
-- * 'awsRegion' - The region authorized to collect aggregated data.
-- * 'lastErrorCode' - The error code that AWS Config returned when the source account aggregation last failed.
-- * 'lastErrorMessage' - The message indicating that the source account aggregation failed due to an error.
-- * 'lastUpdateStatus' - Filters the last updated status type.
--
--
--     * Valid value FAILED indicates errors while moving data.
--
--
--     * Valid value SUCCEEDED indicates the data was successfully moved.
--
--
--     * Valid value OUTDATED indicates the data is not the most recent.
--
--
-- * 'lastUpdateTime' - The time of the last update.
-- * 'sourceId' - The source account ID or an organization.
-- * 'sourceType' - The source account or an organization.
mkAggregatedSourceStatus ::
  AggregatedSourceStatus
mkAggregatedSourceStatus =
  AggregatedSourceStatus'
    { lastErrorCode = Lude.Nothing,
      lastUpdateStatus = Lude.Nothing,
      sourceType = Lude.Nothing,
      sourceId = Lude.Nothing,
      lastErrorMessage = Lude.Nothing,
      awsRegion = Lude.Nothing,
      lastUpdateTime = Lude.Nothing
    }

-- | The error code that AWS Config returned when the source account aggregation last failed.
--
-- /Note:/ Consider using 'lastErrorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assLastErrorCode :: Lens.Lens' AggregatedSourceStatus (Lude.Maybe Lude.Text)
assLastErrorCode = Lens.lens (lastErrorCode :: AggregatedSourceStatus -> Lude.Maybe Lude.Text) (\s a -> s {lastErrorCode = a} :: AggregatedSourceStatus)
{-# DEPRECATED assLastErrorCode "Use generic-lens or generic-optics with 'lastErrorCode' instead." #-}

-- | Filters the last updated status type.
--
--
--     * Valid value FAILED indicates errors while moving data.
--
--
--     * Valid value SUCCEEDED indicates the data was successfully moved.
--
--
--     * Valid value OUTDATED indicates the data is not the most recent.
--
--
--
-- /Note:/ Consider using 'lastUpdateStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assLastUpdateStatus :: Lens.Lens' AggregatedSourceStatus (Lude.Maybe AggregatedSourceStatusType)
assLastUpdateStatus = Lens.lens (lastUpdateStatus :: AggregatedSourceStatus -> Lude.Maybe AggregatedSourceStatusType) (\s a -> s {lastUpdateStatus = a} :: AggregatedSourceStatus)
{-# DEPRECATED assLastUpdateStatus "Use generic-lens or generic-optics with 'lastUpdateStatus' instead." #-}

-- | The source account or an organization.
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assSourceType :: Lens.Lens' AggregatedSourceStatus (Lude.Maybe AggregatedSourceType)
assSourceType = Lens.lens (sourceType :: AggregatedSourceStatus -> Lude.Maybe AggregatedSourceType) (\s a -> s {sourceType = a} :: AggregatedSourceStatus)
{-# DEPRECATED assSourceType "Use generic-lens or generic-optics with 'sourceType' instead." #-}

-- | The source account ID or an organization.
--
-- /Note:/ Consider using 'sourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assSourceId :: Lens.Lens' AggregatedSourceStatus (Lude.Maybe Lude.Text)
assSourceId = Lens.lens (sourceId :: AggregatedSourceStatus -> Lude.Maybe Lude.Text) (\s a -> s {sourceId = a} :: AggregatedSourceStatus)
{-# DEPRECATED assSourceId "Use generic-lens or generic-optics with 'sourceId' instead." #-}

-- | The message indicating that the source account aggregation failed due to an error.
--
-- /Note:/ Consider using 'lastErrorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assLastErrorMessage :: Lens.Lens' AggregatedSourceStatus (Lude.Maybe Lude.Text)
assLastErrorMessage = Lens.lens (lastErrorMessage :: AggregatedSourceStatus -> Lude.Maybe Lude.Text) (\s a -> s {lastErrorMessage = a} :: AggregatedSourceStatus)
{-# DEPRECATED assLastErrorMessage "Use generic-lens or generic-optics with 'lastErrorMessage' instead." #-}

-- | The region authorized to collect aggregated data.
--
-- /Note:/ Consider using 'awsRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assAWSRegion :: Lens.Lens' AggregatedSourceStatus (Lude.Maybe Lude.Text)
assAWSRegion = Lens.lens (awsRegion :: AggregatedSourceStatus -> Lude.Maybe Lude.Text) (\s a -> s {awsRegion = a} :: AggregatedSourceStatus)
{-# DEPRECATED assAWSRegion "Use generic-lens or generic-optics with 'awsRegion' instead." #-}

-- | The time of the last update.
--
-- /Note:/ Consider using 'lastUpdateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assLastUpdateTime :: Lens.Lens' AggregatedSourceStatus (Lude.Maybe Lude.Timestamp)
assLastUpdateTime = Lens.lens (lastUpdateTime :: AggregatedSourceStatus -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdateTime = a} :: AggregatedSourceStatus)
{-# DEPRECATED assLastUpdateTime "Use generic-lens or generic-optics with 'lastUpdateTime' instead." #-}

instance Lude.FromJSON AggregatedSourceStatus where
  parseJSON =
    Lude.withObject
      "AggregatedSourceStatus"
      ( \x ->
          AggregatedSourceStatus'
            Lude.<$> (x Lude..:? "LastErrorCode")
            Lude.<*> (x Lude..:? "LastUpdateStatus")
            Lude.<*> (x Lude..:? "SourceType")
            Lude.<*> (x Lude..:? "SourceId")
            Lude.<*> (x Lude..:? "LastErrorMessage")
            Lude.<*> (x Lude..:? "AwsRegion")
            Lude.<*> (x Lude..:? "LastUpdateTime")
      )
