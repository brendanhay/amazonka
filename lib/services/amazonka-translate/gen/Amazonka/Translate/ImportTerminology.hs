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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a custom terminology, depending on whether or not one
-- already exists for the given terminology name. Importing a terminology
-- with the same name as an existing one will merge the terminologies based
-- on the chosen merge strategy. Currently, the only supported merge
-- strategy is OVERWRITE, and so the imported terminology will overwrite an
-- existing terminology of the same name.
--
-- If you import a terminology that overwrites an existing one, the new
-- terminology take up to 10 minutes to fully propagate and be available
-- for use in a translation due to cache policies with the DataPlane
-- service that performs the translations.
module Amazonka.Translate.ImportTerminology
  ( -- * Creating a Request
    ImportTerminology (..),
    newImportTerminology,

    -- * Request Lenses
    importTerminology_encryptionKey,
    importTerminology_description,
    importTerminology_name,
    importTerminology_mergeStrategy,
    importTerminology_terminologyData,

    -- * Destructuring the Response
    ImportTerminologyResponse (..),
    newImportTerminologyResponse,

    -- * Response Lenses
    importTerminologyResponse_terminologyProperties,
    importTerminologyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Translate.Types

-- | /See:/ 'newImportTerminology' smart constructor.
data ImportTerminology = ImportTerminology'
  { -- | The encryption key for the custom terminology being imported.
    encryptionKey :: Prelude.Maybe EncryptionKey,
    -- | The description of the custom terminology being imported.
    description :: Prelude.Maybe Prelude.Text,
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
-- 'encryptionKey', 'importTerminology_encryptionKey' - The encryption key for the custom terminology being imported.
--
-- 'description', 'importTerminology_description' - The description of the custom terminology being imported.
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
      { encryptionKey = Prelude.Nothing,
        description = Prelude.Nothing,
        name = pName_,
        mergeStrategy = pMergeStrategy_,
        terminologyData = pTerminologyData_
      }

-- | The encryption key for the custom terminology being imported.
importTerminology_encryptionKey :: Lens.Lens' ImportTerminology (Prelude.Maybe EncryptionKey)
importTerminology_encryptionKey = Lens.lens (\ImportTerminology' {encryptionKey} -> encryptionKey) (\s@ImportTerminology' {} a -> s {encryptionKey = a} :: ImportTerminology)

-- | The description of the custom terminology being imported.
importTerminology_description :: Lens.Lens' ImportTerminology (Prelude.Maybe Prelude.Text)
importTerminology_description = Lens.lens (\ImportTerminology' {description} -> description) (\s@ImportTerminology' {} a -> s {description = a} :: ImportTerminology)

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ImportTerminologyResponse'
            Prelude.<$> (x Core..?> "TerminologyProperties")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ImportTerminology where
  hashWithSalt salt' ImportTerminology' {..} =
    salt' `Prelude.hashWithSalt` terminologyData
      `Prelude.hashWithSalt` mergeStrategy
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` encryptionKey

instance Prelude.NFData ImportTerminology where
  rnf ImportTerminology' {..} =
    Prelude.rnf encryptionKey
      `Prelude.seq` Prelude.rnf terminologyData
      `Prelude.seq` Prelude.rnf mergeStrategy
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf description

instance Core.ToHeaders ImportTerminology where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSShineFrontendService_20170701.ImportTerminology" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ImportTerminology where
  toJSON ImportTerminology' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("EncryptionKey" Core..=) Prelude.<$> encryptionKey,
            ("Description" Core..=) Prelude.<$> description,
            Prelude.Just ("Name" Core..= name),
            Prelude.Just ("MergeStrategy" Core..= mergeStrategy),
            Prelude.Just
              ("TerminologyData" Core..= terminologyData)
          ]
      )

instance Core.ToPath ImportTerminology where
  toPath = Prelude.const "/"

instance Core.ToQuery ImportTerminology where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newImportTerminologyResponse' smart constructor.
data ImportTerminologyResponse = ImportTerminologyResponse'
  { -- | The properties of the custom terminology being imported.
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
-- 'terminologyProperties', 'importTerminologyResponse_terminologyProperties' - The properties of the custom terminology being imported.
--
-- 'httpStatus', 'importTerminologyResponse_httpStatus' - The response's http status code.
newImportTerminologyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ImportTerminologyResponse
newImportTerminologyResponse pHttpStatus_ =
  ImportTerminologyResponse'
    { terminologyProperties =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The properties of the custom terminology being imported.
importTerminologyResponse_terminologyProperties :: Lens.Lens' ImportTerminologyResponse (Prelude.Maybe TerminologyProperties)
importTerminologyResponse_terminologyProperties = Lens.lens (\ImportTerminologyResponse' {terminologyProperties} -> terminologyProperties) (\s@ImportTerminologyResponse' {} a -> s {terminologyProperties = a} :: ImportTerminologyResponse)

-- | The response's http status code.
importTerminologyResponse_httpStatus :: Lens.Lens' ImportTerminologyResponse Prelude.Int
importTerminologyResponse_httpStatus = Lens.lens (\ImportTerminologyResponse' {httpStatus} -> httpStatus) (\s@ImportTerminologyResponse' {} a -> s {httpStatus = a} :: ImportTerminologyResponse)

instance Prelude.NFData ImportTerminologyResponse where
  rnf ImportTerminologyResponse' {..} =
    Prelude.rnf terminologyProperties
      `Prelude.seq` Prelude.rnf httpStatus
