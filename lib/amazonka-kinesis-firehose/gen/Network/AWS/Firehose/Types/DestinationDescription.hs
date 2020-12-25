{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.DestinationDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.DestinationDescription
  ( DestinationDescription (..),

    -- * Smart constructor
    mkDestinationDescription,

    -- * Lenses
    ddDestinationId,
    ddElasticsearchDestinationDescription,
    ddExtendedS3DestinationDescription,
    ddHttpEndpointDestinationDescription,
    ddRedshiftDestinationDescription,
    ddS3DestinationDescription,
    ddSplunkDestinationDescription,
  )
where

import qualified Network.AWS.Firehose.Types.DestinationId as Types
import qualified Network.AWS.Firehose.Types.ElasticsearchDestinationDescription as Types
import qualified Network.AWS.Firehose.Types.ExtendedS3DestinationDescription as Types
import qualified Network.AWS.Firehose.Types.HttpEndpointDestinationDescription as Types
import qualified Network.AWS.Firehose.Types.RedshiftDestinationDescription as Types
import qualified Network.AWS.Firehose.Types.S3DestinationDescription as Types
import qualified Network.AWS.Firehose.Types.SplunkDestinationDescription as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the destination for a delivery stream.
--
-- /See:/ 'mkDestinationDescription' smart constructor.
data DestinationDescription = DestinationDescription'
  { -- | The ID of the destination.
    destinationId :: Types.DestinationId,
    -- | The destination in Amazon ES.
    elasticsearchDestinationDescription :: Core.Maybe Types.ElasticsearchDestinationDescription,
    -- | The destination in Amazon S3.
    extendedS3DestinationDescription :: Core.Maybe Types.ExtendedS3DestinationDescription,
    -- | Describes the specified HTTP endpoint destination.
    httpEndpointDestinationDescription :: Core.Maybe Types.HttpEndpointDestinationDescription,
    -- | The destination in Amazon Redshift.
    redshiftDestinationDescription :: Core.Maybe Types.RedshiftDestinationDescription,
    -- | [Deprecated] The destination in Amazon S3.
    s3DestinationDescription :: Core.Maybe Types.S3DestinationDescription,
    -- | The destination in Splunk.
    splunkDestinationDescription :: Core.Maybe Types.SplunkDestinationDescription
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DestinationDescription' value with any optional fields omitted.
mkDestinationDescription ::
  -- | 'destinationId'
  Types.DestinationId ->
  DestinationDescription
mkDestinationDescription destinationId =
  DestinationDescription'
    { destinationId,
      elasticsearchDestinationDescription = Core.Nothing,
      extendedS3DestinationDescription = Core.Nothing,
      httpEndpointDestinationDescription = Core.Nothing,
      redshiftDestinationDescription = Core.Nothing,
      s3DestinationDescription = Core.Nothing,
      splunkDestinationDescription = Core.Nothing
    }

-- | The ID of the destination.
--
-- /Note:/ Consider using 'destinationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDestinationId :: Lens.Lens' DestinationDescription Types.DestinationId
ddDestinationId = Lens.field @"destinationId"
{-# DEPRECATED ddDestinationId "Use generic-lens or generic-optics with 'destinationId' instead." #-}

-- | The destination in Amazon ES.
--
-- /Note:/ Consider using 'elasticsearchDestinationDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddElasticsearchDestinationDescription :: Lens.Lens' DestinationDescription (Core.Maybe Types.ElasticsearchDestinationDescription)
ddElasticsearchDestinationDescription = Lens.field @"elasticsearchDestinationDescription"
{-# DEPRECATED ddElasticsearchDestinationDescription "Use generic-lens or generic-optics with 'elasticsearchDestinationDescription' instead." #-}

-- | The destination in Amazon S3.
--
-- /Note:/ Consider using 'extendedS3DestinationDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddExtendedS3DestinationDescription :: Lens.Lens' DestinationDescription (Core.Maybe Types.ExtendedS3DestinationDescription)
ddExtendedS3DestinationDescription = Lens.field @"extendedS3DestinationDescription"
{-# DEPRECATED ddExtendedS3DestinationDescription "Use generic-lens or generic-optics with 'extendedS3DestinationDescription' instead." #-}

-- | Describes the specified HTTP endpoint destination.
--
-- /Note:/ Consider using 'httpEndpointDestinationDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddHttpEndpointDestinationDescription :: Lens.Lens' DestinationDescription (Core.Maybe Types.HttpEndpointDestinationDescription)
ddHttpEndpointDestinationDescription = Lens.field @"httpEndpointDestinationDescription"
{-# DEPRECATED ddHttpEndpointDestinationDescription "Use generic-lens or generic-optics with 'httpEndpointDestinationDescription' instead." #-}

-- | The destination in Amazon Redshift.
--
-- /Note:/ Consider using 'redshiftDestinationDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddRedshiftDestinationDescription :: Lens.Lens' DestinationDescription (Core.Maybe Types.RedshiftDestinationDescription)
ddRedshiftDestinationDescription = Lens.field @"redshiftDestinationDescription"
{-# DEPRECATED ddRedshiftDestinationDescription "Use generic-lens or generic-optics with 'redshiftDestinationDescription' instead." #-}

-- | [Deprecated] The destination in Amazon S3.
--
-- /Note:/ Consider using 's3DestinationDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddS3DestinationDescription :: Lens.Lens' DestinationDescription (Core.Maybe Types.S3DestinationDescription)
ddS3DestinationDescription = Lens.field @"s3DestinationDescription"
{-# DEPRECATED ddS3DestinationDescription "Use generic-lens or generic-optics with 's3DestinationDescription' instead." #-}

-- | The destination in Splunk.
--
-- /Note:/ Consider using 'splunkDestinationDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddSplunkDestinationDescription :: Lens.Lens' DestinationDescription (Core.Maybe Types.SplunkDestinationDescription)
ddSplunkDestinationDescription = Lens.field @"splunkDestinationDescription"
{-# DEPRECATED ddSplunkDestinationDescription "Use generic-lens or generic-optics with 'splunkDestinationDescription' instead." #-}

instance Core.FromJSON DestinationDescription where
  parseJSON =
    Core.withObject "DestinationDescription" Core.$
      \x ->
        DestinationDescription'
          Core.<$> (x Core..: "DestinationId")
          Core.<*> (x Core..:? "ElasticsearchDestinationDescription")
          Core.<*> (x Core..:? "ExtendedS3DestinationDescription")
          Core.<*> (x Core..:? "HttpEndpointDestinationDescription")
          Core.<*> (x Core..:? "RedshiftDestinationDescription")
          Core.<*> (x Core..:? "S3DestinationDescription")
          Core.<*> (x Core..:? "SplunkDestinationDescription")
