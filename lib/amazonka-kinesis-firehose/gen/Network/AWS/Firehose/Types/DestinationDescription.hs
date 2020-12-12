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
    ddSplunkDestinationDescription,
    ddHTTPEndpointDestinationDescription,
    ddS3DestinationDescription,
    ddExtendedS3DestinationDescription,
    ddElasticsearchDestinationDescription,
    ddRedshiftDestinationDescription,
    ddDestinationId,
  )
where

import Network.AWS.Firehose.Types.ElasticsearchDestinationDescription
import Network.AWS.Firehose.Types.ExtendedS3DestinationDescription
import Network.AWS.Firehose.Types.HTTPEndpointDestinationDescription
import Network.AWS.Firehose.Types.RedshiftDestinationDescription
import Network.AWS.Firehose.Types.S3DestinationDescription
import Network.AWS.Firehose.Types.SplunkDestinationDescription
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the destination for a delivery stream.
--
-- /See:/ 'mkDestinationDescription' smart constructor.
data DestinationDescription = DestinationDescription'
  { splunkDestinationDescription ::
      Lude.Maybe SplunkDestinationDescription,
    hTTPEndpointDestinationDescription ::
      Lude.Maybe HTTPEndpointDestinationDescription,
    s3DestinationDescription ::
      Lude.Maybe S3DestinationDescription,
    extendedS3DestinationDescription ::
      Lude.Maybe ExtendedS3DestinationDescription,
    elasticsearchDestinationDescription ::
      Lude.Maybe
        ElasticsearchDestinationDescription,
    redshiftDestinationDescription ::
      Lude.Maybe RedshiftDestinationDescription,
    destinationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DestinationDescription' with the minimum fields required to make a request.
--
-- * 'destinationId' - The ID of the destination.
-- * 'elasticsearchDestinationDescription' - The destination in Amazon ES.
-- * 'extendedS3DestinationDescription' - The destination in Amazon S3.
-- * 'hTTPEndpointDestinationDescription' - Describes the specified HTTP endpoint destination.
-- * 'redshiftDestinationDescription' - The destination in Amazon Redshift.
-- * 's3DestinationDescription' - [Deprecated] The destination in Amazon S3.
-- * 'splunkDestinationDescription' - The destination in Splunk.
mkDestinationDescription ::
  -- | 'destinationId'
  Lude.Text ->
  DestinationDescription
mkDestinationDescription pDestinationId_ =
  DestinationDescription'
    { splunkDestinationDescription =
        Lude.Nothing,
      hTTPEndpointDestinationDescription = Lude.Nothing,
      s3DestinationDescription = Lude.Nothing,
      extendedS3DestinationDescription = Lude.Nothing,
      elasticsearchDestinationDescription = Lude.Nothing,
      redshiftDestinationDescription = Lude.Nothing,
      destinationId = pDestinationId_
    }

-- | The destination in Splunk.
--
-- /Note:/ Consider using 'splunkDestinationDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddSplunkDestinationDescription :: Lens.Lens' DestinationDescription (Lude.Maybe SplunkDestinationDescription)
ddSplunkDestinationDescription = Lens.lens (splunkDestinationDescription :: DestinationDescription -> Lude.Maybe SplunkDestinationDescription) (\s a -> s {splunkDestinationDescription = a} :: DestinationDescription)
{-# DEPRECATED ddSplunkDestinationDescription "Use generic-lens or generic-optics with 'splunkDestinationDescription' instead." #-}

-- | Describes the specified HTTP endpoint destination.
--
-- /Note:/ Consider using 'hTTPEndpointDestinationDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddHTTPEndpointDestinationDescription :: Lens.Lens' DestinationDescription (Lude.Maybe HTTPEndpointDestinationDescription)
ddHTTPEndpointDestinationDescription = Lens.lens (hTTPEndpointDestinationDescription :: DestinationDescription -> Lude.Maybe HTTPEndpointDestinationDescription) (\s a -> s {hTTPEndpointDestinationDescription = a} :: DestinationDescription)
{-# DEPRECATED ddHTTPEndpointDestinationDescription "Use generic-lens or generic-optics with 'hTTPEndpointDestinationDescription' instead." #-}

-- | [Deprecated] The destination in Amazon S3.
--
-- /Note:/ Consider using 's3DestinationDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddS3DestinationDescription :: Lens.Lens' DestinationDescription (Lude.Maybe S3DestinationDescription)
ddS3DestinationDescription = Lens.lens (s3DestinationDescription :: DestinationDescription -> Lude.Maybe S3DestinationDescription) (\s a -> s {s3DestinationDescription = a} :: DestinationDescription)
{-# DEPRECATED ddS3DestinationDescription "Use generic-lens or generic-optics with 's3DestinationDescription' instead." #-}

-- | The destination in Amazon S3.
--
-- /Note:/ Consider using 'extendedS3DestinationDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddExtendedS3DestinationDescription :: Lens.Lens' DestinationDescription (Lude.Maybe ExtendedS3DestinationDescription)
ddExtendedS3DestinationDescription = Lens.lens (extendedS3DestinationDescription :: DestinationDescription -> Lude.Maybe ExtendedS3DestinationDescription) (\s a -> s {extendedS3DestinationDescription = a} :: DestinationDescription)
{-# DEPRECATED ddExtendedS3DestinationDescription "Use generic-lens or generic-optics with 'extendedS3DestinationDescription' instead." #-}

-- | The destination in Amazon ES.
--
-- /Note:/ Consider using 'elasticsearchDestinationDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddElasticsearchDestinationDescription :: Lens.Lens' DestinationDescription (Lude.Maybe ElasticsearchDestinationDescription)
ddElasticsearchDestinationDescription = Lens.lens (elasticsearchDestinationDescription :: DestinationDescription -> Lude.Maybe ElasticsearchDestinationDescription) (\s a -> s {elasticsearchDestinationDescription = a} :: DestinationDescription)
{-# DEPRECATED ddElasticsearchDestinationDescription "Use generic-lens or generic-optics with 'elasticsearchDestinationDescription' instead." #-}

-- | The destination in Amazon Redshift.
--
-- /Note:/ Consider using 'redshiftDestinationDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddRedshiftDestinationDescription :: Lens.Lens' DestinationDescription (Lude.Maybe RedshiftDestinationDescription)
ddRedshiftDestinationDescription = Lens.lens (redshiftDestinationDescription :: DestinationDescription -> Lude.Maybe RedshiftDestinationDescription) (\s a -> s {redshiftDestinationDescription = a} :: DestinationDescription)
{-# DEPRECATED ddRedshiftDestinationDescription "Use generic-lens or generic-optics with 'redshiftDestinationDescription' instead." #-}

-- | The ID of the destination.
--
-- /Note:/ Consider using 'destinationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDestinationId :: Lens.Lens' DestinationDescription Lude.Text
ddDestinationId = Lens.lens (destinationId :: DestinationDescription -> Lude.Text) (\s a -> s {destinationId = a} :: DestinationDescription)
{-# DEPRECATED ddDestinationId "Use generic-lens or generic-optics with 'destinationId' instead." #-}

instance Lude.FromJSON DestinationDescription where
  parseJSON =
    Lude.withObject
      "DestinationDescription"
      ( \x ->
          DestinationDescription'
            Lude.<$> (x Lude..:? "SplunkDestinationDescription")
            Lude.<*> (x Lude..:? "HttpEndpointDestinationDescription")
            Lude.<*> (x Lude..:? "S3DestinationDescription")
            Lude.<*> (x Lude..:? "ExtendedS3DestinationDescription")
            Lude.<*> (x Lude..:? "ElasticsearchDestinationDescription")
            Lude.<*> (x Lude..:? "RedshiftDestinationDescription")
            Lude.<*> (x Lude..: "DestinationId")
      )
