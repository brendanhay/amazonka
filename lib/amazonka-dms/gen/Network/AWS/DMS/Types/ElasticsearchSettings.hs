{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    esServiceAccessRoleArn,
    esEndpointUri,
    esErrorRetryDuration,
    esFullLoadErrorPercentage,
  )
where

import qualified Network.AWS.DMS.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information that defines an Elasticsearch endpoint.
--
-- /See:/ 'mkElasticsearchSettings' smart constructor.
data ElasticsearchSettings = ElasticsearchSettings'
  { -- | The Amazon Resource Name (ARN) used by service to access the IAM role.
    serviceAccessRoleArn :: Types.String,
    -- | The endpoint for the Elasticsearch cluster. AWS DMS uses HTTPS if a transport protocol (http/https) is not specified.
    endpointUri :: Types.String,
    -- | The maximum number of seconds for which DMS retries failed API requests to the Elasticsearch cluster.
    errorRetryDuration :: Core.Maybe Core.Int,
    -- | The maximum percentage of records that can fail to be written before a full load operation stops.
    --
    -- To avoid early failure, this counter is only effective after 1000 records are transferred. Elasticsearch also has the concept of error monitoring during the last 10 minutes of an Observation Window. If transfer of all records fail in the last 10 minutes, the full load operation stops.
    fullLoadErrorPercentage :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ElasticsearchSettings' value with any optional fields omitted.
mkElasticsearchSettings ::
  -- | 'serviceAccessRoleArn'
  Types.String ->
  -- | 'endpointUri'
  Types.String ->
  ElasticsearchSettings
mkElasticsearchSettings serviceAccessRoleArn endpointUri =
  ElasticsearchSettings'
    { serviceAccessRoleArn,
      endpointUri,
      errorRetryDuration = Core.Nothing,
      fullLoadErrorPercentage = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) used by service to access the IAM role.
--
-- /Note:/ Consider using 'serviceAccessRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esServiceAccessRoleArn :: Lens.Lens' ElasticsearchSettings Types.String
esServiceAccessRoleArn = Lens.field @"serviceAccessRoleArn"
{-# DEPRECATED esServiceAccessRoleArn "Use generic-lens or generic-optics with 'serviceAccessRoleArn' instead." #-}

-- | The endpoint for the Elasticsearch cluster. AWS DMS uses HTTPS if a transport protocol (http/https) is not specified.
--
-- /Note:/ Consider using 'endpointUri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esEndpointUri :: Lens.Lens' ElasticsearchSettings Types.String
esEndpointUri = Lens.field @"endpointUri"
{-# DEPRECATED esEndpointUri "Use generic-lens or generic-optics with 'endpointUri' instead." #-}

-- | The maximum number of seconds for which DMS retries failed API requests to the Elasticsearch cluster.
--
-- /Note:/ Consider using 'errorRetryDuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esErrorRetryDuration :: Lens.Lens' ElasticsearchSettings (Core.Maybe Core.Int)
esErrorRetryDuration = Lens.field @"errorRetryDuration"
{-# DEPRECATED esErrorRetryDuration "Use generic-lens or generic-optics with 'errorRetryDuration' instead." #-}

-- | The maximum percentage of records that can fail to be written before a full load operation stops.
--
-- To avoid early failure, this counter is only effective after 1000 records are transferred. Elasticsearch also has the concept of error monitoring during the last 10 minutes of an Observation Window. If transfer of all records fail in the last 10 minutes, the full load operation stops.
--
-- /Note:/ Consider using 'fullLoadErrorPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esFullLoadErrorPercentage :: Lens.Lens' ElasticsearchSettings (Core.Maybe Core.Int)
esFullLoadErrorPercentage = Lens.field @"fullLoadErrorPercentage"
{-# DEPRECATED esFullLoadErrorPercentage "Use generic-lens or generic-optics with 'fullLoadErrorPercentage' instead." #-}

instance Core.FromJSON ElasticsearchSettings where
  toJSON ElasticsearchSettings {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ServiceAccessRoleArn" Core..= serviceAccessRoleArn),
            Core.Just ("EndpointUri" Core..= endpointUri),
            ("ErrorRetryDuration" Core..=) Core.<$> errorRetryDuration,
            ("FullLoadErrorPercentage" Core..=)
              Core.<$> fullLoadErrorPercentage
          ]
      )

instance Core.FromJSON ElasticsearchSettings where
  parseJSON =
    Core.withObject "ElasticsearchSettings" Core.$
      \x ->
        ElasticsearchSettings'
          Core.<$> (x Core..: "ServiceAccessRoleArn")
          Core.<*> (x Core..: "EndpointUri")
          Core.<*> (x Core..:? "ErrorRetryDuration")
          Core.<*> (x Core..:? "FullLoadErrorPercentage")
