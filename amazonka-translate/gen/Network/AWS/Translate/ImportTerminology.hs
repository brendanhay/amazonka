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
-- Module      : Network.AWS.Translate.ImportTerminology
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
module Network.AWS.Translate.ImportTerminology
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Translate.Types

-- | /See:/ 'newImportTerminology' smart constructor.
data ImportTerminology = ImportTerminology'
  { -- | The encryption key for the custom terminology being imported.
    encryptionKey :: Core.Maybe EncryptionKey,
    -- | The description of the custom terminology being imported.
    description :: Core.Maybe Core.Text,
    -- | The name of the custom terminology being imported.
    name :: Core.Text,
    -- | The merge strategy of the custom terminology being imported. Currently,
    -- only the OVERWRITE merge strategy is supported. In this case, the
    -- imported terminology will overwrite an existing terminology of the same
    -- name.
    mergeStrategy :: MergeStrategy,
    -- | The terminology data for the custom terminology being imported.
    terminologyData :: TerminologyData
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
  Core.Text ->
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
      { encryptionKey = Core.Nothing,
        description = Core.Nothing,
        name = pName_,
        mergeStrategy = pMergeStrategy_,
        terminologyData = pTerminologyData_
      }

-- | The encryption key for the custom terminology being imported.
importTerminology_encryptionKey :: Lens.Lens' ImportTerminology (Core.Maybe EncryptionKey)
importTerminology_encryptionKey = Lens.lens (\ImportTerminology' {encryptionKey} -> encryptionKey) (\s@ImportTerminology' {} a -> s {encryptionKey = a} :: ImportTerminology)

-- | The description of the custom terminology being imported.
importTerminology_description :: Lens.Lens' ImportTerminology (Core.Maybe Core.Text)
importTerminology_description = Lens.lens (\ImportTerminology' {description} -> description) (\s@ImportTerminology' {} a -> s {description = a} :: ImportTerminology)

-- | The name of the custom terminology being imported.
importTerminology_name :: Lens.Lens' ImportTerminology Core.Text
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
            Core.<$> (x Core..?> "TerminologyProperties")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ImportTerminology

instance Core.NFData ImportTerminology

instance Core.ToHeaders ImportTerminology where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSShineFrontendService_20170701.ImportTerminology" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ImportTerminology where
  toJSON ImportTerminology' {..} =
    Core.object
      ( Core.catMaybes
          [ ("EncryptionKey" Core..=) Core.<$> encryptionKey,
            ("Description" Core..=) Core.<$> description,
            Core.Just ("Name" Core..= name),
            Core.Just ("MergeStrategy" Core..= mergeStrategy),
            Core.Just
              ("TerminologyData" Core..= terminologyData)
          ]
      )

instance Core.ToPath ImportTerminology where
  toPath = Core.const "/"

instance Core.ToQuery ImportTerminology where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newImportTerminologyResponse' smart constructor.
data ImportTerminologyResponse = ImportTerminologyResponse'
  { -- | The properties of the custom terminology being imported.
    terminologyProperties :: Core.Maybe TerminologyProperties,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ImportTerminologyResponse
newImportTerminologyResponse pHttpStatus_ =
  ImportTerminologyResponse'
    { terminologyProperties =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The properties of the custom terminology being imported.
importTerminologyResponse_terminologyProperties :: Lens.Lens' ImportTerminologyResponse (Core.Maybe TerminologyProperties)
importTerminologyResponse_terminologyProperties = Lens.lens (\ImportTerminologyResponse' {terminologyProperties} -> terminologyProperties) (\s@ImportTerminologyResponse' {} a -> s {terminologyProperties = a} :: ImportTerminologyResponse)

-- | The response's http status code.
importTerminologyResponse_httpStatus :: Lens.Lens' ImportTerminologyResponse Core.Int
importTerminologyResponse_httpStatus = Lens.lens (\ImportTerminologyResponse' {httpStatus} -> httpStatus) (\s@ImportTerminologyResponse' {} a -> s {httpStatus = a} :: ImportTerminologyResponse)

instance Core.NFData ImportTerminologyResponse
