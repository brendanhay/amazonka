{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Translate.ImportTerminology
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a custom terminology, depending on whether one
-- already exists for the given terminology name. Importing a terminology
-- with the same name as an existing one will merge the terminologies based
-- on the chosen merge strategy. The only supported merge strategy is
-- OVERWRITE, where the imported terminology overwrites the existing
-- terminology of the same name.
--
-- If you import a terminology that overwrites an existing one, the new
-- terminology takes up to 10 minutes to fully propagate. After that,
-- translations have access to the new terminology.
module Amazonka.Translate.ImportTerminology
  ( -- * Creating a Request
    ImportTerminology (..),
    newImportTerminology,

    -- * Request Lenses
    importTerminology_description,
    importTerminology_encryptionKey,
    importTerminology_tags,
    importTerminology_name,
    importTerminology_mergeStrategy,
    importTerminology_terminologyData,

    -- * Destructuring the Response
    ImportTerminologyResponse (..),
    newImportTerminologyResponse,

    -- * Response Lenses
    importTerminologyResponse_auxiliaryDataLocation,
    importTerminologyResponse_terminologyProperties,
    importTerminologyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Translate.Types

-- | /See:/ 'newImportTerminology' smart constructor.
data ImportTerminology = ImportTerminology'
  { -- | The description of the custom terminology being imported.
    description :: Prelude.Maybe Prelude.Text,
    -- | The encryption key for the custom terminology being imported.
    encryptionKey :: Prelude.Maybe EncryptionKey,
    -- | Tags to be associated with this resource. A tag is a key-value pair that
    -- adds metadata to a resource. Each tag key for the resource must be
    -- unique. For more information, see
    -- <https://docs.aws.amazon.com/translate/latest/dg/tagging.html Tagging your resources>.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the custom terminology being imported.
    name :: Prelude.Text,
    -- | The merge strategy of the custom terminology being imported. Currently,
    -- only the OVERWRITE merge strategy is supported. In this case, the
    -- imported terminology will overwrite an existing terminology of the same
    -- name.
    mergeStrategy :: MergeStrategy,
    -- | The terminology data for the custom terminology being imported.
    terminologyData :: TerminologyData
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportTerminology' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'importTerminology_description' - The description of the custom terminology being imported.
--
-- 'encryptionKey', 'importTerminology_encryptionKey' - The encryption key for the custom terminology being imported.
--
-- 'tags', 'importTerminology_tags' - Tags to be associated with this resource. A tag is a key-value pair that
-- adds metadata to a resource. Each tag key for the resource must be
-- unique. For more information, see
-- <https://docs.aws.amazon.com/translate/latest/dg/tagging.html Tagging your resources>.
--
-- 'name', 'importTerminology_name' - The name of the custom terminology being imported.
--
-- 'mergeStrategy', 'importTerminology_mergeStrategy' - The merge strategy of the custom terminology being imported. Currently,
-- only the OVERWRITE merge strategy is supported. In this case, the
-- imported terminology will overwrite an existing terminology of the same
-- name.
--
-- 'terminologyData', 'importTerminology_terminologyData' - The terminology data for the custom terminology being imported.
newImportTerminology ::
  -- | 'name'
  Prelude.Text ->
  -- | 'mergeStrategy'
  MergeStrategy ->
  -- | 'terminologyData'
  TerminologyData ->
  ImportTerminology
newImportTerminology
  pName_
  pMergeStrategy_
  pTerminologyData_ =
    ImportTerminology'
      { description = Prelude.Nothing,
        encryptionKey = Prelude.Nothing,
        tags = Prelude.Nothing,
        name = pName_,
        mergeStrategy = pMergeStrategy_,
        terminologyData = pTerminologyData_
      }

-- | The description of the custom terminology being imported.
importTerminology_description :: Lens.Lens' ImportTerminology (Prelude.Maybe Prelude.Text)
importTerminology_description = Lens.lens (\ImportTerminology' {description} -> description) (\s@ImportTerminology' {} a -> s {description = a} :: ImportTerminology)

-- | The encryption key for the custom terminology being imported.
importTerminology_encryptionKey :: Lens.Lens' ImportTerminology (Prelude.Maybe EncryptionKey)
importTerminology_encryptionKey = Lens.lens (\ImportTerminology' {encryptionKey} -> encryptionKey) (\s@ImportTerminology' {} a -> s {encryptionKey = a} :: ImportTerminology)

-- | Tags to be associated with this resource. A tag is a key-value pair that
-- adds metadata to a resource. Each tag key for the resource must be
-- unique. For more information, see
-- <https://docs.aws.amazon.com/translate/latest/dg/tagging.html Tagging your resources>.
importTerminology_tags :: Lens.Lens' ImportTerminology (Prelude.Maybe [Tag])
importTerminology_tags = Lens.lens (\ImportTerminology' {tags} -> tags) (\s@ImportTerminology' {} a -> s {tags = a} :: ImportTerminology) Prelude.. Lens.mapping Lens.coerced

-- | The name of the custom terminology being imported.
importTerminology_name :: Lens.Lens' ImportTerminology Prelude.Text
importTerminology_name = Lens.lens (\ImportTerminology' {name} -> name) (\s@ImportTerminology' {} a -> s {name = a} :: ImportTerminology)

-- | The merge strategy of the custom terminology being imported. Currently,
-- only the OVERWRITE merge strategy is supported. In this case, the
-- imported terminology will overwrite an existing terminology of the same
-- name.
importTerminology_mergeStrategy :: Lens.Lens' ImportTerminology MergeStrategy
importTerminology_mergeStrategy = Lens.lens (\ImportTerminology' {mergeStrategy} -> mergeStrategy) (\s@ImportTerminology' {} a -> s {mergeStrategy = a} :: ImportTerminology)

-- | The terminology data for the custom terminology being imported.
importTerminology_terminologyData :: Lens.Lens' ImportTerminology TerminologyData
importTerminology_terminologyData = Lens.lens (\ImportTerminology' {terminologyData} -> terminologyData) (\s@ImportTerminology' {} a -> s {terminologyData = a} :: ImportTerminology)

instance Core.AWSRequest ImportTerminology where
  type
    AWSResponse ImportTerminology =
      ImportTerminologyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ImportTerminologyResponse'
            Prelude.<$> (x Data..?> "AuxiliaryDataLocation")
            Prelude.<*> (x Data..?> "TerminologyProperties")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ImportTerminology where
  hashWithSalt _salt ImportTerminology' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` encryptionKey
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` mergeStrategy
      `Prelude.hashWithSalt` terminologyData

instance Prelude.NFData ImportTerminology where
  rnf ImportTerminology' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf encryptionKey `Prelude.seq`
        Prelude.rnf tags `Prelude.seq`
          Prelude.rnf name `Prelude.seq`
            Prelude.rnf mergeStrategy `Prelude.seq`
              Prelude.rnf terminologyData

instance Data.ToHeaders ImportTerminology where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSShineFrontendService_20170701.ImportTerminology" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ImportTerminology where
  toJSON ImportTerminology' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("EncryptionKey" Data..=) Prelude.<$> encryptionKey,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("MergeStrategy" Data..= mergeStrategy),
            Prelude.Just
              ("TerminologyData" Data..= terminologyData)
          ]
      )

instance Data.ToPath ImportTerminology where
  toPath = Prelude.const "/"

instance Data.ToQuery ImportTerminology where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newImportTerminologyResponse' smart constructor.
data ImportTerminologyResponse = ImportTerminologyResponse'
  { -- | The Amazon S3 location of a file that provides any errors or warnings
    -- that were produced by your input file. This file was created when Amazon
    -- Translate attempted to create a terminology resource. The location is
    -- returned as a presigned URL to that has a 30 minute expiration.
    auxiliaryDataLocation :: Prelude.Maybe TerminologyDataLocation,
    -- | The properties of the custom terminology being imported.
    terminologyProperties :: Prelude.Maybe TerminologyProperties,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportTerminologyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'auxiliaryDataLocation', 'importTerminologyResponse_auxiliaryDataLocation' - The Amazon S3 location of a file that provides any errors or warnings
-- that were produced by your input file. This file was created when Amazon
-- Translate attempted to create a terminology resource. The location is
-- returned as a presigned URL to that has a 30 minute expiration.
--
-- 'terminologyProperties', 'importTerminologyResponse_terminologyProperties' - The properties of the custom terminology being imported.
--
-- 'httpStatus', 'importTerminologyResponse_httpStatus' - The response's http status code.
newImportTerminologyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ImportTerminologyResponse
newImportTerminologyResponse pHttpStatus_ =
  ImportTerminologyResponse'
    { auxiliaryDataLocation =
        Prelude.Nothing,
      terminologyProperties = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon S3 location of a file that provides any errors or warnings
-- that were produced by your input file. This file was created when Amazon
-- Translate attempted to create a terminology resource. The location is
-- returned as a presigned URL to that has a 30 minute expiration.
importTerminologyResponse_auxiliaryDataLocation :: Lens.Lens' ImportTerminologyResponse (Prelude.Maybe TerminologyDataLocation)
importTerminologyResponse_auxiliaryDataLocation = Lens.lens (\ImportTerminologyResponse' {auxiliaryDataLocation} -> auxiliaryDataLocation) (\s@ImportTerminologyResponse' {} a -> s {auxiliaryDataLocation = a} :: ImportTerminologyResponse)

-- | The properties of the custom terminology being imported.
importTerminologyResponse_terminologyProperties :: Lens.Lens' ImportTerminologyResponse (Prelude.Maybe TerminologyProperties)
importTerminologyResponse_terminologyProperties = Lens.lens (\ImportTerminologyResponse' {terminologyProperties} -> terminologyProperties) (\s@ImportTerminologyResponse' {} a -> s {terminologyProperties = a} :: ImportTerminologyResponse)

-- | The response's http status code.
importTerminologyResponse_httpStatus :: Lens.Lens' ImportTerminologyResponse Prelude.Int
importTerminologyResponse_httpStatus = Lens.lens (\ImportTerminologyResponse' {httpStatus} -> httpStatus) (\s@ImportTerminologyResponse' {} a -> s {httpStatus = a} :: ImportTerminologyResponse)

instance Prelude.NFData ImportTerminologyResponse where
  rnf ImportTerminologyResponse' {..} =
    Prelude.rnf auxiliaryDataLocation `Prelude.seq`
      Prelude.rnf terminologyProperties `Prelude.seq`
        Prelude.rnf httpStatus
