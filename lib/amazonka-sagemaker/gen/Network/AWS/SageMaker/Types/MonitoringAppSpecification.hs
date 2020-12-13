{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.MonitoringAppSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MonitoringAppSpecification
  ( MonitoringAppSpecification (..),

    -- * Smart constructor
    mkMonitoringAppSpecification,

    -- * Lenses
    masContainerArguments,
    masRecordPreprocessorSourceURI,
    masImageURI,
    masContainerEntrypoint,
    masPostAnalyticsProcessorSourceURI,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Container image configuration object for the monitoring job.
--
-- /See:/ 'mkMonitoringAppSpecification' smart constructor.
data MonitoringAppSpecification = MonitoringAppSpecification'
  { -- | An array of arguments for the container used to run the monitoring job.
    containerArguments :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    -- | An Amazon S3 URI to a script that is called per row prior to running analysis. It can base64 decode the payload and convert it into a flatted json so that the built-in container can use the converted data. Applicable only for the built-in (first party) containers.
    recordPreprocessorSourceURI :: Lude.Maybe Lude.Text,
    -- | The container image to be run by the monitoring job.
    imageURI :: Lude.Text,
    -- | Specifies the entrypoint for a container used to run the monitoring job.
    containerEntrypoint :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    -- | An Amazon S3 URI to a script that is called after analysis has been performed. Applicable only for the built-in (first party) containers.
    postAnalyticsProcessorSourceURI :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MonitoringAppSpecification' with the minimum fields required to make a request.
--
-- * 'containerArguments' - An array of arguments for the container used to run the monitoring job.
-- * 'recordPreprocessorSourceURI' - An Amazon S3 URI to a script that is called per row prior to running analysis. It can base64 decode the payload and convert it into a flatted json so that the built-in container can use the converted data. Applicable only for the built-in (first party) containers.
-- * 'imageURI' - The container image to be run by the monitoring job.
-- * 'containerEntrypoint' - Specifies the entrypoint for a container used to run the monitoring job.
-- * 'postAnalyticsProcessorSourceURI' - An Amazon S3 URI to a script that is called after analysis has been performed. Applicable only for the built-in (first party) containers.
mkMonitoringAppSpecification ::
  -- | 'imageURI'
  Lude.Text ->
  MonitoringAppSpecification
mkMonitoringAppSpecification pImageURI_ =
  MonitoringAppSpecification'
    { containerArguments = Lude.Nothing,
      recordPreprocessorSourceURI = Lude.Nothing,
      imageURI = pImageURI_,
      containerEntrypoint = Lude.Nothing,
      postAnalyticsProcessorSourceURI = Lude.Nothing
    }

-- | An array of arguments for the container used to run the monitoring job.
--
-- /Note:/ Consider using 'containerArguments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
masContainerArguments :: Lens.Lens' MonitoringAppSpecification (Lude.Maybe (Lude.NonEmpty Lude.Text))
masContainerArguments = Lens.lens (containerArguments :: MonitoringAppSpecification -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {containerArguments = a} :: MonitoringAppSpecification)
{-# DEPRECATED masContainerArguments "Use generic-lens or generic-optics with 'containerArguments' instead." #-}

-- | An Amazon S3 URI to a script that is called per row prior to running analysis. It can base64 decode the payload and convert it into a flatted json so that the built-in container can use the converted data. Applicable only for the built-in (first party) containers.
--
-- /Note:/ Consider using 'recordPreprocessorSourceURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
masRecordPreprocessorSourceURI :: Lens.Lens' MonitoringAppSpecification (Lude.Maybe Lude.Text)
masRecordPreprocessorSourceURI = Lens.lens (recordPreprocessorSourceURI :: MonitoringAppSpecification -> Lude.Maybe Lude.Text) (\s a -> s {recordPreprocessorSourceURI = a} :: MonitoringAppSpecification)
{-# DEPRECATED masRecordPreprocessorSourceURI "Use generic-lens or generic-optics with 'recordPreprocessorSourceURI' instead." #-}

-- | The container image to be run by the monitoring job.
--
-- /Note:/ Consider using 'imageURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
masImageURI :: Lens.Lens' MonitoringAppSpecification Lude.Text
masImageURI = Lens.lens (imageURI :: MonitoringAppSpecification -> Lude.Text) (\s a -> s {imageURI = a} :: MonitoringAppSpecification)
{-# DEPRECATED masImageURI "Use generic-lens or generic-optics with 'imageURI' instead." #-}

-- | Specifies the entrypoint for a container used to run the monitoring job.
--
-- /Note:/ Consider using 'containerEntrypoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
masContainerEntrypoint :: Lens.Lens' MonitoringAppSpecification (Lude.Maybe (Lude.NonEmpty Lude.Text))
masContainerEntrypoint = Lens.lens (containerEntrypoint :: MonitoringAppSpecification -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {containerEntrypoint = a} :: MonitoringAppSpecification)
{-# DEPRECATED masContainerEntrypoint "Use generic-lens or generic-optics with 'containerEntrypoint' instead." #-}

-- | An Amazon S3 URI to a script that is called after analysis has been performed. Applicable only for the built-in (first party) containers.
--
-- /Note:/ Consider using 'postAnalyticsProcessorSourceURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
masPostAnalyticsProcessorSourceURI :: Lens.Lens' MonitoringAppSpecification (Lude.Maybe Lude.Text)
masPostAnalyticsProcessorSourceURI = Lens.lens (postAnalyticsProcessorSourceURI :: MonitoringAppSpecification -> Lude.Maybe Lude.Text) (\s a -> s {postAnalyticsProcessorSourceURI = a} :: MonitoringAppSpecification)
{-# DEPRECATED masPostAnalyticsProcessorSourceURI "Use generic-lens or generic-optics with 'postAnalyticsProcessorSourceURI' instead." #-}

instance Lude.FromJSON MonitoringAppSpecification where
  parseJSON =
    Lude.withObject
      "MonitoringAppSpecification"
      ( \x ->
          MonitoringAppSpecification'
            Lude.<$> (x Lude..:? "ContainerArguments")
            Lude.<*> (x Lude..:? "RecordPreprocessorSourceUri")
            Lude.<*> (x Lude..: "ImageUri")
            Lude.<*> (x Lude..:? "ContainerEntrypoint")
            Lude.<*> (x Lude..:? "PostAnalyticsProcessorSourceUri")
      )

instance Lude.ToJSON MonitoringAppSpecification where
  toJSON MonitoringAppSpecification' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ContainerArguments" Lude..=) Lude.<$> containerArguments,
            ("RecordPreprocessorSourceUri" Lude..=)
              Lude.<$> recordPreprocessorSourceURI,
            Lude.Just ("ImageUri" Lude..= imageURI),
            ("ContainerEntrypoint" Lude..=) Lude.<$> containerEntrypoint,
            ("PostAnalyticsProcessorSourceUri" Lude..=)
              Lude.<$> postAnalyticsProcessorSourceURI
          ]
      )
