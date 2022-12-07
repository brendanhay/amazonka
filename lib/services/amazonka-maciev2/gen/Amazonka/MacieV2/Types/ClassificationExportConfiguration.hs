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
-- Module      : Amazonka.MacieV2.Types.ClassificationExportConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.ClassificationExportConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.S3Destination
import qualified Amazonka.Prelude as Prelude

-- | Specifies where to store data classification results, and the encryption
-- settings to use when storing results in that location. Currently, you
-- can store classification results only in an S3 bucket.
--
-- /See:/ 'newClassificationExportConfiguration' smart constructor.
data ClassificationExportConfiguration = ClassificationExportConfiguration'
  { -- | The S3 bucket to store data classification results in, and the
    -- encryption settings to use when storing results in that bucket.
    s3Destination :: Prelude.Maybe S3Destination
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClassificationExportConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Destination', 'classificationExportConfiguration_s3Destination' - The S3 bucket to store data classification results in, and the
-- encryption settings to use when storing results in that bucket.
newClassificationExportConfiguration ::
  ClassificationExportConfiguration
newClassificationExportConfiguration =
  ClassificationExportConfiguration'
    { s3Destination =
        Prelude.Nothing
    }

-- | The S3 bucket to store data classification results in, and the
-- encryption settings to use when storing results in that bucket.
classificationExportConfiguration_s3Destination :: Lens.Lens' ClassificationExportConfiguration (Prelude.Maybe S3Destination)
classificationExportConfiguration_s3Destination = Lens.lens (\ClassificationExportConfiguration' {s3Destination} -> s3Destination) (\s@ClassificationExportConfiguration' {} a -> s {s3Destination = a} :: ClassificationExportConfiguration)

instance
  Data.FromJSON
    ClassificationExportConfiguration
  where
  parseJSON =
    Data.withObject
      "ClassificationExportConfiguration"
      ( \x ->
          ClassificationExportConfiguration'
            Prelude.<$> (x Data..:? "s3Destination")
      )

instance
  Prelude.Hashable
    ClassificationExportConfiguration
  where
  hashWithSalt
    _salt
    ClassificationExportConfiguration' {..} =
      _salt `Prelude.hashWithSalt` s3Destination

instance
  Prelude.NFData
    ClassificationExportConfiguration
  where
  rnf ClassificationExportConfiguration' {..} =
    Prelude.rnf s3Destination

instance
  Data.ToJSON
    ClassificationExportConfiguration
  where
  toJSON ClassificationExportConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("s3Destination" Data..=)
              Prelude.<$> s3Destination
          ]
      )
