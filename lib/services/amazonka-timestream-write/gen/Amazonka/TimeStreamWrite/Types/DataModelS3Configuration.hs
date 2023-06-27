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
-- Module      : Amazonka.TimeStreamWrite.Types.DataModelS3Configuration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamWrite.Types.DataModelS3Configuration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- |
--
-- /See:/ 'newDataModelS3Configuration' smart constructor.
data DataModelS3Configuration = DataModelS3Configuration'
  { bucketName :: Prelude.Maybe Prelude.Text,
    objectKey :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataModelS3Configuration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucketName', 'dataModelS3Configuration_bucketName' -
--
-- 'objectKey', 'dataModelS3Configuration_objectKey' -
newDataModelS3Configuration ::
  DataModelS3Configuration
newDataModelS3Configuration =
  DataModelS3Configuration'
    { bucketName =
        Prelude.Nothing,
      objectKey = Prelude.Nothing
    }

dataModelS3Configuration_bucketName :: Lens.Lens' DataModelS3Configuration (Prelude.Maybe Prelude.Text)
dataModelS3Configuration_bucketName = Lens.lens (\DataModelS3Configuration' {bucketName} -> bucketName) (\s@DataModelS3Configuration' {} a -> s {bucketName = a} :: DataModelS3Configuration)

dataModelS3Configuration_objectKey :: Lens.Lens' DataModelS3Configuration (Prelude.Maybe Prelude.Text)
dataModelS3Configuration_objectKey = Lens.lens (\DataModelS3Configuration' {objectKey} -> objectKey) (\s@DataModelS3Configuration' {} a -> s {objectKey = a} :: DataModelS3Configuration)

instance Data.FromJSON DataModelS3Configuration where
  parseJSON =
    Data.withObject
      "DataModelS3Configuration"
      ( \x ->
          DataModelS3Configuration'
            Prelude.<$> (x Data..:? "BucketName")
            Prelude.<*> (x Data..:? "ObjectKey")
      )

instance Prelude.Hashable DataModelS3Configuration where
  hashWithSalt _salt DataModelS3Configuration' {..} =
    _salt
      `Prelude.hashWithSalt` bucketName
      `Prelude.hashWithSalt` objectKey

instance Prelude.NFData DataModelS3Configuration where
  rnf DataModelS3Configuration' {..} =
    Prelude.rnf bucketName
      `Prelude.seq` Prelude.rnf objectKey

instance Data.ToJSON DataModelS3Configuration where
  toJSON DataModelS3Configuration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BucketName" Data..=) Prelude.<$> bucketName,
            ("ObjectKey" Data..=) Prelude.<$> objectKey
          ]
      )
