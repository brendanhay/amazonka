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
-- Module      : Amazonka.Firehose.Types.DestinationDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.DestinationDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Firehose.Types.AmazonOpenSearchServerlessDestinationDescription
import Amazonka.Firehose.Types.AmazonopensearchserviceDestinationDescription
import Amazonka.Firehose.Types.ElasticsearchDestinationDescription
import Amazonka.Firehose.Types.ExtendedS3DestinationDescription
import Amazonka.Firehose.Types.HttpEndpointDestinationDescription
import Amazonka.Firehose.Types.RedshiftDestinationDescription
import Amazonka.Firehose.Types.S3DestinationDescription
import Amazonka.Firehose.Types.SplunkDestinationDescription
import qualified Amazonka.Prelude as Prelude

-- | Describes the destination for a delivery stream.
--
-- /See:/ 'newDestinationDescription' smart constructor.
data DestinationDescription = DestinationDescription'
  { -- | The destination in the Serverless offering for Amazon OpenSearch
    -- Service.
    amazonOpenSearchServerlessDestinationDescription :: Prelude.Maybe AmazonOpenSearchServerlessDestinationDescription,
    -- | The destination in Amazon OpenSearch Service.
    amazonopensearchserviceDestinationDescription :: Prelude.Maybe AmazonopensearchserviceDestinationDescription,
    -- | The destination in Amazon ES.
    elasticsearchDestinationDescription :: Prelude.Maybe ElasticsearchDestinationDescription,
    -- | The destination in Amazon S3.
    extendedS3DestinationDescription :: Prelude.Maybe ExtendedS3DestinationDescription,
    -- | Describes the specified HTTP endpoint destination.
    httpEndpointDestinationDescription :: Prelude.Maybe HttpEndpointDestinationDescription,
    -- | The destination in Amazon Redshift.
    redshiftDestinationDescription :: Prelude.Maybe RedshiftDestinationDescription,
    -- | [Deprecated] The destination in Amazon S3.
    s3DestinationDescription :: Prelude.Maybe S3DestinationDescription,
    -- | The destination in Splunk.
    splunkDestinationDescription :: Prelude.Maybe SplunkDestinationDescription,
    -- | The ID of the destination.
    destinationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DestinationDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'amazonOpenSearchServerlessDestinationDescription', 'destinationDescription_amazonOpenSearchServerlessDestinationDescription' - The destination in the Serverless offering for Amazon OpenSearch
-- Service.
--
-- 'amazonopensearchserviceDestinationDescription', 'destinationDescription_amazonopensearchserviceDestinationDescription' - The destination in Amazon OpenSearch Service.
--
-- 'elasticsearchDestinationDescription', 'destinationDescription_elasticsearchDestinationDescription' - The destination in Amazon ES.
--
-- 'extendedS3DestinationDescription', 'destinationDescription_extendedS3DestinationDescription' - The destination in Amazon S3.
--
-- 'httpEndpointDestinationDescription', 'destinationDescription_httpEndpointDestinationDescription' - Describes the specified HTTP endpoint destination.
--
-- 'redshiftDestinationDescription', 'destinationDescription_redshiftDestinationDescription' - The destination in Amazon Redshift.
--
-- 's3DestinationDescription', 'destinationDescription_s3DestinationDescription' - [Deprecated] The destination in Amazon S3.
--
-- 'splunkDestinationDescription', 'destinationDescription_splunkDestinationDescription' - The destination in Splunk.
--
-- 'destinationId', 'destinationDescription_destinationId' - The ID of the destination.
newDestinationDescription ::
  -- | 'destinationId'
  Prelude.Text ->
  DestinationDescription
newDestinationDescription pDestinationId_ =
  DestinationDescription'
    { amazonOpenSearchServerlessDestinationDescription =
        Prelude.Nothing,
      amazonopensearchserviceDestinationDescription =
        Prelude.Nothing,
      elasticsearchDestinationDescription =
        Prelude.Nothing,
      extendedS3DestinationDescription = Prelude.Nothing,
      httpEndpointDestinationDescription =
        Prelude.Nothing,
      redshiftDestinationDescription = Prelude.Nothing,
      s3DestinationDescription = Prelude.Nothing,
      splunkDestinationDescription = Prelude.Nothing,
      destinationId = pDestinationId_
    }

-- | The destination in the Serverless offering for Amazon OpenSearch
-- Service.
destinationDescription_amazonOpenSearchServerlessDestinationDescription :: Lens.Lens' DestinationDescription (Prelude.Maybe AmazonOpenSearchServerlessDestinationDescription)
destinationDescription_amazonOpenSearchServerlessDestinationDescription = Lens.lens (\DestinationDescription' {amazonOpenSearchServerlessDestinationDescription} -> amazonOpenSearchServerlessDestinationDescription) (\s@DestinationDescription' {} a -> s {amazonOpenSearchServerlessDestinationDescription = a} :: DestinationDescription)

-- | The destination in Amazon OpenSearch Service.
destinationDescription_amazonopensearchserviceDestinationDescription :: Lens.Lens' DestinationDescription (Prelude.Maybe AmazonopensearchserviceDestinationDescription)
destinationDescription_amazonopensearchserviceDestinationDescription = Lens.lens (\DestinationDescription' {amazonopensearchserviceDestinationDescription} -> amazonopensearchserviceDestinationDescription) (\s@DestinationDescription' {} a -> s {amazonopensearchserviceDestinationDescription = a} :: DestinationDescription)

-- | The destination in Amazon ES.
destinationDescription_elasticsearchDestinationDescription :: Lens.Lens' DestinationDescription (Prelude.Maybe ElasticsearchDestinationDescription)
destinationDescription_elasticsearchDestinationDescription = Lens.lens (\DestinationDescription' {elasticsearchDestinationDescription} -> elasticsearchDestinationDescription) (\s@DestinationDescription' {} a -> s {elasticsearchDestinationDescription = a} :: DestinationDescription)

-- | The destination in Amazon S3.
destinationDescription_extendedS3DestinationDescription :: Lens.Lens' DestinationDescription (Prelude.Maybe ExtendedS3DestinationDescription)
destinationDescription_extendedS3DestinationDescription = Lens.lens (\DestinationDescription' {extendedS3DestinationDescription} -> extendedS3DestinationDescription) (\s@DestinationDescription' {} a -> s {extendedS3DestinationDescription = a} :: DestinationDescription)

-- | Describes the specified HTTP endpoint destination.
destinationDescription_httpEndpointDestinationDescription :: Lens.Lens' DestinationDescription (Prelude.Maybe HttpEndpointDestinationDescription)
destinationDescription_httpEndpointDestinationDescription = Lens.lens (\DestinationDescription' {httpEndpointDestinationDescription} -> httpEndpointDestinationDescription) (\s@DestinationDescription' {} a -> s {httpEndpointDestinationDescription = a} :: DestinationDescription)

-- | The destination in Amazon Redshift.
destinationDescription_redshiftDestinationDescription :: Lens.Lens' DestinationDescription (Prelude.Maybe RedshiftDestinationDescription)
destinationDescription_redshiftDestinationDescription = Lens.lens (\DestinationDescription' {redshiftDestinationDescription} -> redshiftDestinationDescription) (\s@DestinationDescription' {} a -> s {redshiftDestinationDescription = a} :: DestinationDescription)

-- | [Deprecated] The destination in Amazon S3.
destinationDescription_s3DestinationDescription :: Lens.Lens' DestinationDescription (Prelude.Maybe S3DestinationDescription)
destinationDescription_s3DestinationDescription = Lens.lens (\DestinationDescription' {s3DestinationDescription} -> s3DestinationDescription) (\s@DestinationDescription' {} a -> s {s3DestinationDescription = a} :: DestinationDescription)

-- | The destination in Splunk.
destinationDescription_splunkDestinationDescription :: Lens.Lens' DestinationDescription (Prelude.Maybe SplunkDestinationDescription)
destinationDescription_splunkDestinationDescription = Lens.lens (\DestinationDescription' {splunkDestinationDescription} -> splunkDestinationDescription) (\s@DestinationDescription' {} a -> s {splunkDestinationDescription = a} :: DestinationDescription)

-- | The ID of the destination.
destinationDescription_destinationId :: Lens.Lens' DestinationDescription Prelude.Text
destinationDescription_destinationId = Lens.lens (\DestinationDescription' {destinationId} -> destinationId) (\s@DestinationDescription' {} a -> s {destinationId = a} :: DestinationDescription)

instance Data.FromJSON DestinationDescription where
  parseJSON =
    Data.withObject
      "DestinationDescription"
      ( \x ->
          DestinationDescription'
            Prelude.<$> ( x
                            Data..:? "AmazonOpenSearchServerlessDestinationDescription"
                        )
            Prelude.<*> ( x
                            Data..:? "AmazonopensearchserviceDestinationDescription"
                        )
            Prelude.<*> (x Data..:? "ElasticsearchDestinationDescription")
            Prelude.<*> (x Data..:? "ExtendedS3DestinationDescription")
            Prelude.<*> (x Data..:? "HttpEndpointDestinationDescription")
            Prelude.<*> (x Data..:? "RedshiftDestinationDescription")
            Prelude.<*> (x Data..:? "S3DestinationDescription")
            Prelude.<*> (x Data..:? "SplunkDestinationDescription")
            Prelude.<*> (x Data..: "DestinationId")
      )

instance Prelude.Hashable DestinationDescription where
  hashWithSalt _salt DestinationDescription' {..} =
    _salt
      `Prelude.hashWithSalt` amazonOpenSearchServerlessDestinationDescription
      `Prelude.hashWithSalt` amazonopensearchserviceDestinationDescription
      `Prelude.hashWithSalt` elasticsearchDestinationDescription
      `Prelude.hashWithSalt` extendedS3DestinationDescription
      `Prelude.hashWithSalt` httpEndpointDestinationDescription
      `Prelude.hashWithSalt` redshiftDestinationDescription
      `Prelude.hashWithSalt` s3DestinationDescription
      `Prelude.hashWithSalt` splunkDestinationDescription
      `Prelude.hashWithSalt` destinationId

instance Prelude.NFData DestinationDescription where
  rnf DestinationDescription' {..} =
    Prelude.rnf
      amazonOpenSearchServerlessDestinationDescription
      `Prelude.seq` Prelude.rnf
        amazonopensearchserviceDestinationDescription
      `Prelude.seq` Prelude.rnf elasticsearchDestinationDescription
      `Prelude.seq` Prelude.rnf extendedS3DestinationDescription
      `Prelude.seq` Prelude.rnf httpEndpointDestinationDescription
      `Prelude.seq` Prelude.rnf redshiftDestinationDescription
      `Prelude.seq` Prelude.rnf s3DestinationDescription
      `Prelude.seq` Prelude.rnf splunkDestinationDescription
      `Prelude.seq` Prelude.rnf destinationId
