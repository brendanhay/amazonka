-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.ElasticsearchSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.ElasticsearchSettings
  ( ElasticsearchSettings (..),

    -- * Smart constructor
    mkElasticsearchSettings,

    -- * Lenses
    esFullLoadErrorPercentage,
    esErrorRetryDuration,
    esServiceAccessRoleARN,
    esEndpointURI,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information that defines an Elasticsearch endpoint.
--
-- /See:/ 'mkElasticsearchSettings' smart constructor.
data ElasticsearchSettings = ElasticsearchSettings'
  { fullLoadErrorPercentage ::
      Lude.Maybe Lude.Int,
    errorRetryDuration :: Lude.Maybe Lude.Int,
    serviceAccessRoleARN :: Lude.Text,
    endpointURI :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ElasticsearchSettings' with the minimum fields required to make a request.
--
-- * 'endpointURI' - The endpoint for the Elasticsearch cluster. AWS DMS uses HTTPS if a transport protocol (http/https) is not specified.
-- * 'errorRetryDuration' - The maximum number of seconds for which DMS retries failed API requests to the Elasticsearch cluster.
-- * 'fullLoadErrorPercentage' - The maximum percentage of records that can fail to be written before a full load operation stops.
--
-- To avoid early failure, this counter is only effective after 1000 records are transferred. Elasticsearch also has the concept of error monitoring during the last 10 minutes of an Observation Window. If transfer of all records fail in the last 10 minutes, the full load operation stops.
-- * 'serviceAccessRoleARN' - The Amazon Resource Name (ARN) used by service to access the IAM role.
mkElasticsearchSettings ::
  -- | 'serviceAccessRoleARN'
  Lude.Text ->
  -- | 'endpointURI'
  Lude.Text ->
  ElasticsearchSettings
mkElasticsearchSettings pServiceAccessRoleARN_ pEndpointURI_ =
  ElasticsearchSettings'
    { fullLoadErrorPercentage = Lude.Nothing,
      errorRetryDuration = Lude.Nothing,
      serviceAccessRoleARN = pServiceAccessRoleARN_,
      endpointURI = pEndpointURI_
    }

-- | The maximum percentage of records that can fail to be written before a full load operation stops.
--
-- To avoid early failure, this counter is only effective after 1000 records are transferred. Elasticsearch also has the concept of error monitoring during the last 10 minutes of an Observation Window. If transfer of all records fail in the last 10 minutes, the full load operation stops.
--
-- /Note:/ Consider using 'fullLoadErrorPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esFullLoadErrorPercentage :: Lens.Lens' ElasticsearchSettings (Lude.Maybe Lude.Int)
esFullLoadErrorPercentage = Lens.lens (fullLoadErrorPercentage :: ElasticsearchSettings -> Lude.Maybe Lude.Int) (\s a -> s {fullLoadErrorPercentage = a} :: ElasticsearchSettings)
{-# DEPRECATED esFullLoadErrorPercentage "Use generic-lens or generic-optics with 'fullLoadErrorPercentage' instead." #-}

-- | The maximum number of seconds for which DMS retries failed API requests to the Elasticsearch cluster.
--
-- /Note:/ Consider using 'errorRetryDuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esErrorRetryDuration :: Lens.Lens' ElasticsearchSettings (Lude.Maybe Lude.Int)
esErrorRetryDuration = Lens.lens (errorRetryDuration :: ElasticsearchSettings -> Lude.Maybe Lude.Int) (\s a -> s {errorRetryDuration = a} :: ElasticsearchSettings)
{-# DEPRECATED esErrorRetryDuration "Use generic-lens or generic-optics with 'errorRetryDuration' instead." #-}

-- | The Amazon Resource Name (ARN) used by service to access the IAM role.
--
-- /Note:/ Consider using 'serviceAccessRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esServiceAccessRoleARN :: Lens.Lens' ElasticsearchSettings Lude.Text
esServiceAccessRoleARN = Lens.lens (serviceAccessRoleARN :: ElasticsearchSettings -> Lude.Text) (\s a -> s {serviceAccessRoleARN = a} :: ElasticsearchSettings)
{-# DEPRECATED esServiceAccessRoleARN "Use generic-lens or generic-optics with 'serviceAccessRoleARN' instead." #-}

-- | The endpoint for the Elasticsearch cluster. AWS DMS uses HTTPS if a transport protocol (http/https) is not specified.
--
-- /Note:/ Consider using 'endpointURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esEndpointURI :: Lens.Lens' ElasticsearchSettings Lude.Text
esEndpointURI = Lens.lens (endpointURI :: ElasticsearchSettings -> Lude.Text) (\s a -> s {endpointURI = a} :: ElasticsearchSettings)
{-# DEPRECATED esEndpointURI "Use generic-lens or generic-optics with 'endpointURI' instead." #-}

instance Lude.FromJSON ElasticsearchSettings where
  parseJSON =
    Lude.withObject
      "ElasticsearchSettings"
      ( \x ->
          ElasticsearchSettings'
            Lude.<$> (x Lude..:? "FullLoadErrorPercentage")
            Lude.<*> (x Lude..:? "ErrorRetryDuration")
            Lude.<*> (x Lude..: "ServiceAccessRoleArn")
            Lude.<*> (x Lude..: "EndpointUri")
      )

instance Lude.ToJSON ElasticsearchSettings where
  toJSON ElasticsearchSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("FullLoadErrorPercentage" Lude..=)
              Lude.<$> fullLoadErrorPercentage,
            ("ErrorRetryDuration" Lude..=) Lude.<$> errorRetryDuration,
            Lude.Just ("ServiceAccessRoleArn" Lude..= serviceAccessRoleARN),
            Lude.Just ("EndpointUri" Lude..= endpointURI)
          ]
      )
