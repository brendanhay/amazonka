{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.ApplicationUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.ApplicationUpdate
  ( ApplicationUpdate (..),

    -- * Smart constructor
    mkApplicationUpdate,

    -- * Lenses
    auReferenceDataSourceUpdates,
    auInputUpdates,
    auCloudWatchLoggingOptionUpdates,
    auOutputUpdates,
    auApplicationCodeUpdate,
  )
where

import Network.AWS.KinesisAnalytics.Types.CloudWatchLoggingOptionUpdate
import Network.AWS.KinesisAnalytics.Types.InputUpdate
import Network.AWS.KinesisAnalytics.Types.OutputUpdate
import Network.AWS.KinesisAnalytics.Types.ReferenceDataSourceUpdate
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes updates to apply to an existing Amazon Kinesis Analytics application.
--
-- /See:/ 'mkApplicationUpdate' smart constructor.
data ApplicationUpdate = ApplicationUpdate'
  { -- | Describes application reference data source updates.
    referenceDataSourceUpdates :: Lude.Maybe [ReferenceDataSourceUpdate],
    -- | Describes application input configuration updates.
    inputUpdates :: Lude.Maybe [InputUpdate],
    -- | Describes application CloudWatch logging option updates.
    cloudWatchLoggingOptionUpdates :: Lude.Maybe [CloudWatchLoggingOptionUpdate],
    -- | Describes application output configuration updates.
    outputUpdates :: Lude.Maybe [OutputUpdate],
    -- | Describes application code updates.
    applicationCodeUpdate :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ApplicationUpdate' with the minimum fields required to make a request.
--
-- * 'referenceDataSourceUpdates' - Describes application reference data source updates.
-- * 'inputUpdates' - Describes application input configuration updates.
-- * 'cloudWatchLoggingOptionUpdates' - Describes application CloudWatch logging option updates.
-- * 'outputUpdates' - Describes application output configuration updates.
-- * 'applicationCodeUpdate' - Describes application code updates.
mkApplicationUpdate ::
  ApplicationUpdate
mkApplicationUpdate =
  ApplicationUpdate'
    { referenceDataSourceUpdates = Lude.Nothing,
      inputUpdates = Lude.Nothing,
      cloudWatchLoggingOptionUpdates = Lude.Nothing,
      outputUpdates = Lude.Nothing,
      applicationCodeUpdate = Lude.Nothing
    }

-- | Describes application reference data source updates.
--
-- /Note:/ Consider using 'referenceDataSourceUpdates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
auReferenceDataSourceUpdates :: Lens.Lens' ApplicationUpdate (Lude.Maybe [ReferenceDataSourceUpdate])
auReferenceDataSourceUpdates = Lens.lens (referenceDataSourceUpdates :: ApplicationUpdate -> Lude.Maybe [ReferenceDataSourceUpdate]) (\s a -> s {referenceDataSourceUpdates = a} :: ApplicationUpdate)
{-# DEPRECATED auReferenceDataSourceUpdates "Use generic-lens or generic-optics with 'referenceDataSourceUpdates' instead." #-}

-- | Describes application input configuration updates.
--
-- /Note:/ Consider using 'inputUpdates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
auInputUpdates :: Lens.Lens' ApplicationUpdate (Lude.Maybe [InputUpdate])
auInputUpdates = Lens.lens (inputUpdates :: ApplicationUpdate -> Lude.Maybe [InputUpdate]) (\s a -> s {inputUpdates = a} :: ApplicationUpdate)
{-# DEPRECATED auInputUpdates "Use generic-lens or generic-optics with 'inputUpdates' instead." #-}

-- | Describes application CloudWatch logging option updates.
--
-- /Note:/ Consider using 'cloudWatchLoggingOptionUpdates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
auCloudWatchLoggingOptionUpdates :: Lens.Lens' ApplicationUpdate (Lude.Maybe [CloudWatchLoggingOptionUpdate])
auCloudWatchLoggingOptionUpdates = Lens.lens (cloudWatchLoggingOptionUpdates :: ApplicationUpdate -> Lude.Maybe [CloudWatchLoggingOptionUpdate]) (\s a -> s {cloudWatchLoggingOptionUpdates = a} :: ApplicationUpdate)
{-# DEPRECATED auCloudWatchLoggingOptionUpdates "Use generic-lens or generic-optics with 'cloudWatchLoggingOptionUpdates' instead." #-}

-- | Describes application output configuration updates.
--
-- /Note:/ Consider using 'outputUpdates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
auOutputUpdates :: Lens.Lens' ApplicationUpdate (Lude.Maybe [OutputUpdate])
auOutputUpdates = Lens.lens (outputUpdates :: ApplicationUpdate -> Lude.Maybe [OutputUpdate]) (\s a -> s {outputUpdates = a} :: ApplicationUpdate)
{-# DEPRECATED auOutputUpdates "Use generic-lens or generic-optics with 'outputUpdates' instead." #-}

-- | Describes application code updates.
--
-- /Note:/ Consider using 'applicationCodeUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
auApplicationCodeUpdate :: Lens.Lens' ApplicationUpdate (Lude.Maybe Lude.Text)
auApplicationCodeUpdate = Lens.lens (applicationCodeUpdate :: ApplicationUpdate -> Lude.Maybe Lude.Text) (\s a -> s {applicationCodeUpdate = a} :: ApplicationUpdate)
{-# DEPRECATED auApplicationCodeUpdate "Use generic-lens or generic-optics with 'applicationCodeUpdate' instead." #-}

instance Lude.ToJSON ApplicationUpdate where
  toJSON ApplicationUpdate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ReferenceDataSourceUpdates" Lude..=)
              Lude.<$> referenceDataSourceUpdates,
            ("InputUpdates" Lude..=) Lude.<$> inputUpdates,
            ("CloudWatchLoggingOptionUpdates" Lude..=)
              Lude.<$> cloudWatchLoggingOptionUpdates,
            ("OutputUpdates" Lude..=) Lude.<$> outputUpdates,
            ("ApplicationCodeUpdate" Lude..=) Lude.<$> applicationCodeUpdate
          ]
      )
