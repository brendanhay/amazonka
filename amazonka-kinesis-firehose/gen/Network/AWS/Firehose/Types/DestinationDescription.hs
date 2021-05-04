{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.DestinationDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.DestinationDescription where

import Network.AWS.Firehose.Types.ElasticsearchDestinationDescription
import Network.AWS.Firehose.Types.ExtendedS3DestinationDescription
import Network.AWS.Firehose.Types.HttpEndpointDestinationDescription
import Network.AWS.Firehose.Types.RedshiftDestinationDescription
import Network.AWS.Firehose.Types.S3DestinationDescription
import Network.AWS.Firehose.Types.SplunkDestinationDescription
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the destination for a delivery stream.
--
-- /See:/ 'newDestinationDescription' smart constructor.
data DestinationDescription = DestinationDescription'
  { -- | The destination in Amazon ES.
    elasticsearchDestinationDescription :: Prelude.Maybe ElasticsearchDestinationDescription,
    -- | Describes the specified HTTP endpoint destination.
    httpEndpointDestinationDescription :: Prelude.Maybe HttpEndpointDestinationDescription,
    -- | The destination in Amazon S3.
    extendedS3DestinationDescription :: Prelude.Maybe ExtendedS3DestinationDescription,
    -- | The destination in Amazon Redshift.
    redshiftDestinationDescription :: Prelude.Maybe RedshiftDestinationDescription,
    -- | The destination in Splunk.
    splunkDestinationDescription :: Prelude.Maybe SplunkDestinationDescription,
    -- | [Deprecated] The destination in Amazon S3.
    s3DestinationDescription :: Prelude.Maybe S3DestinationDescription,
    -- | The ID of the destination.
    destinationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DestinationDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'elasticsearchDestinationDescription', 'destinationDescription_elasticsearchDestinationDescription' - The destination in Amazon ES.
--
-- 'httpEndpointDestinationDescription', 'destinationDescription_httpEndpointDestinationDescription' - Describes the specified HTTP endpoint destination.
--
-- 'extendedS3DestinationDescription', 'destinationDescription_extendedS3DestinationDescription' - The destination in Amazon S3.
--
-- 'redshiftDestinationDescription', 'destinationDescription_redshiftDestinationDescription' - The destination in Amazon Redshift.
--
-- 'splunkDestinationDescription', 'destinationDescription_splunkDestinationDescription' - The destination in Splunk.
--
-- 's3DestinationDescription', 'destinationDescription_s3DestinationDescription' - [Deprecated] The destination in Amazon S3.
--
-- 'destinationId', 'destinationDescription_destinationId' - The ID of the destination.
newDestinationDescription ::
  -- | 'destinationId'
  Prelude.Text ->
  DestinationDescription
newDestinationDescription pDestinationId_ =
  DestinationDescription'
    { elasticsearchDestinationDescription =
        Prelude.Nothing,
      httpEndpointDestinationDescription =
        Prelude.Nothing,
      extendedS3DestinationDescription = Prelude.Nothing,
      redshiftDestinationDescription = Prelude.Nothing,
      splunkDestinationDescription = Prelude.Nothing,
      s3DestinationDescription = Prelude.Nothing,
      destinationId = pDestinationId_
    }

-- | The destination in Amazon ES.
destinationDescription_elasticsearchDestinationDescription :: Lens.Lens' DestinationDescription (Prelude.Maybe ElasticsearchDestinationDescription)
destinationDescription_elasticsearchDestinationDescription = Lens.lens (\DestinationDescription' {elasticsearchDestinationDescription} -> elasticsearchDestinationDescription) (\s@DestinationDescription' {} a -> s {elasticsearchDestinationDescription = a} :: DestinationDescription)

-- | Describes the specified HTTP endpoint destination.
destinationDescription_httpEndpointDestinationDescription :: Lens.Lens' DestinationDescription (Prelude.Maybe HttpEndpointDestinationDescription)
destinationDescription_httpEndpointDestinationDescription = Lens.lens (\DestinationDescription' {httpEndpointDestinationDescription} -> httpEndpointDestinationDescription) (\s@DestinationDescription' {} a -> s {httpEndpointDestinationDescription = a} :: DestinationDescription)

-- | The destination in Amazon S3.
destinationDescription_extendedS3DestinationDescription :: Lens.Lens' DestinationDescription (Prelude.Maybe ExtendedS3DestinationDescription)
destinationDescription_extendedS3DestinationDescription = Lens.lens (\DestinationDescription' {extendedS3DestinationDescription} -> extendedS3DestinationDescription) (\s@DestinationDescription' {} a -> s {extendedS3DestinationDescription = a} :: DestinationDescription)

-- | The destination in Amazon Redshift.
destinationDescription_redshiftDestinationDescription :: Lens.Lens' DestinationDescription (Prelude.Maybe RedshiftDestinationDescription)
destinationDescription_redshiftDestinationDescription = Lens.lens (\DestinationDescription' {redshiftDestinationDescription} -> redshiftDestinationDescription) (\s@DestinationDescription' {} a -> s {redshiftDestinationDescription = a} :: DestinationDescription)

-- | The destination in Splunk.
destinationDescription_splunkDestinationDescription :: Lens.Lens' DestinationDescription (Prelude.Maybe SplunkDestinationDescription)
destinationDescription_splunkDestinationDescription = Lens.lens (\DestinationDescription' {splunkDestinationDescription} -> splunkDestinationDescription) (\s@DestinationDescription' {} a -> s {splunkDestinationDescription = a} :: DestinationDescription)

-- | [Deprecated] The destination in Amazon S3.
destinationDescription_s3DestinationDescription :: Lens.Lens' DestinationDescription (Prelude.Maybe S3DestinationDescription)
destinationDescription_s3DestinationDescription = Lens.lens (\DestinationDescription' {s3DestinationDescription} -> s3DestinationDescription) (\s@DestinationDescription' {} a -> s {s3DestinationDescription = a} :: DestinationDescription)

-- | The ID of the destination.
destinationDescription_destinationId :: Lens.Lens' DestinationDescription Prelude.Text
destinationDescription_destinationId = Lens.lens (\DestinationDescription' {destinationId} -> destinationId) (\s@DestinationDescription' {} a -> s {destinationId = a} :: DestinationDescription)

instance Prelude.FromJSON DestinationDescription where
  parseJSON =
    Prelude.withObject
      "DestinationDescription"
      ( \x ->
          DestinationDescription'
            Prelude.<$> (x Prelude..:? "ElasticsearchDestinationDescription")
            Prelude.<*> (x Prelude..:? "HttpEndpointDestinationDescription")
            Prelude.<*> (x Prelude..:? "ExtendedS3DestinationDescription")
            Prelude.<*> (x Prelude..:? "RedshiftDestinationDescription")
            Prelude.<*> (x Prelude..:? "SplunkDestinationDescription")
            Prelude.<*> (x Prelude..:? "S3DestinationDescription")
            Prelude.<*> (x Prelude..: "DestinationId")
      )

instance Prelude.Hashable DestinationDescription

instance Prelude.NFData DestinationDescription
