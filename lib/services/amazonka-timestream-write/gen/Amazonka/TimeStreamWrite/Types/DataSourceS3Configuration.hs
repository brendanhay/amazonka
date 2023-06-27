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
-- Module      : Amazonka.TimeStreamWrite.Types.DataSourceS3Configuration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamWrite.Types.DataSourceS3Configuration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- |
--
-- /See:/ 'newDataSourceS3Configuration' smart constructor.
data DataSourceS3Configuration = DataSourceS3Configuration'
  { objectKeyPrefix :: Prelude.Maybe Prelude.Text,
    -- | The bucket name of the customer S3 bucket.
    bucketName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataSourceS3Configuration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'objectKeyPrefix', 'dataSourceS3Configuration_objectKeyPrefix' -
--
-- 'bucketName', 'dataSourceS3Configuration_bucketName' - The bucket name of the customer S3 bucket.
newDataSourceS3Configuration ::
  -- | 'bucketName'
  Prelude.Text ->
  DataSourceS3Configuration
newDataSourceS3Configuration pBucketName_ =
  DataSourceS3Configuration'
    { objectKeyPrefix =
        Prelude.Nothing,
      bucketName = pBucketName_
    }

dataSourceS3Configuration_objectKeyPrefix :: Lens.Lens' DataSourceS3Configuration (Prelude.Maybe Prelude.Text)
dataSourceS3Configuration_objectKeyPrefix = Lens.lens (\DataSourceS3Configuration' {objectKeyPrefix} -> objectKeyPrefix) (\s@DataSourceS3Configuration' {} a -> s {objectKeyPrefix = a} :: DataSourceS3Configuration)

-- | The bucket name of the customer S3 bucket.
dataSourceS3Configuration_bucketName :: Lens.Lens' DataSourceS3Configuration Prelude.Text
dataSourceS3Configuration_bucketName = Lens.lens (\DataSourceS3Configuration' {bucketName} -> bucketName) (\s@DataSourceS3Configuration' {} a -> s {bucketName = a} :: DataSourceS3Configuration)

instance Data.FromJSON DataSourceS3Configuration where
  parseJSON =
    Data.withObject
      "DataSourceS3Configuration"
      ( \x ->
          DataSourceS3Configuration'
            Prelude.<$> (x Data..:? "ObjectKeyPrefix")
            Prelude.<*> (x Data..: "BucketName")
      )

instance Prelude.Hashable DataSourceS3Configuration where
  hashWithSalt _salt DataSourceS3Configuration' {..} =
    _salt
      `Prelude.hashWithSalt` objectKeyPrefix
      `Prelude.hashWithSalt` bucketName

instance Prelude.NFData DataSourceS3Configuration where
  rnf DataSourceS3Configuration' {..} =
    Prelude.rnf objectKeyPrefix
      `Prelude.seq` Prelude.rnf bucketName

instance Data.ToJSON DataSourceS3Configuration where
  toJSON DataSourceS3Configuration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ObjectKeyPrefix" Data..=)
              Prelude.<$> objectKeyPrefix,
            Prelude.Just ("BucketName" Data..= bucketName)
          ]
      )
