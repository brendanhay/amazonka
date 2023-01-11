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
-- Module      : Amazonka.DirectoryService.StartSchemaExtension
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Applies a schema extension to a Microsoft AD directory.
module Amazonka.DirectoryService.StartSchemaExtension
  ( -- * Creating a Request
    StartSchemaExtension (..),
    newStartSchemaExtension,

    -- * Request Lenses
    startSchemaExtension_directoryId,
    startSchemaExtension_createSnapshotBeforeSchemaExtension,
    startSchemaExtension_ldifContent,
    startSchemaExtension_description,

    -- * Destructuring the Response
    StartSchemaExtensionResponse (..),
    newStartSchemaExtensionResponse,

    -- * Response Lenses
    startSchemaExtensionResponse_schemaExtensionId,
    startSchemaExtensionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectoryService.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartSchemaExtension' smart constructor.
data StartSchemaExtension = StartSchemaExtension'
  { -- | The identifier of the directory for which the schema extension will be
    -- applied to.
    directoryId :: Prelude.Text,
    -- | If true, creates a snapshot of the directory before applying the schema
    -- extension.
    createSnapshotBeforeSchemaExtension :: Prelude.Bool,
    -- | The LDIF file represented as a string. To construct the LdifContent
    -- string, precede each line as it would be formatted in an ldif file with
    -- \\n. See the example request below for more details. The file size can
    -- be no larger than 1MB.
    ldifContent :: Prelude.Text,
    -- | A description of the schema extension.
    description :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartSchemaExtension' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryId', 'startSchemaExtension_directoryId' - The identifier of the directory for which the schema extension will be
-- applied to.
--
-- 'createSnapshotBeforeSchemaExtension', 'startSchemaExtension_createSnapshotBeforeSchemaExtension' - If true, creates a snapshot of the directory before applying the schema
-- extension.
--
-- 'ldifContent', 'startSchemaExtension_ldifContent' - The LDIF file represented as a string. To construct the LdifContent
-- string, precede each line as it would be formatted in an ldif file with
-- \\n. See the example request below for more details. The file size can
-- be no larger than 1MB.
--
-- 'description', 'startSchemaExtension_description' - A description of the schema extension.
newStartSchemaExtension ::
  -- | 'directoryId'
  Prelude.Text ->
  -- | 'createSnapshotBeforeSchemaExtension'
  Prelude.Bool ->
  -- | 'ldifContent'
  Prelude.Text ->
  -- | 'description'
  Prelude.Text ->
  StartSchemaExtension
newStartSchemaExtension
  pDirectoryId_
  pCreateSnapshotBeforeSchemaExtension_
  pLdifContent_
  pDescription_ =
    StartSchemaExtension'
      { directoryId = pDirectoryId_,
        createSnapshotBeforeSchemaExtension =
          pCreateSnapshotBeforeSchemaExtension_,
        ldifContent = pLdifContent_,
        description = pDescription_
      }

-- | The identifier of the directory for which the schema extension will be
-- applied to.
startSchemaExtension_directoryId :: Lens.Lens' StartSchemaExtension Prelude.Text
startSchemaExtension_directoryId = Lens.lens (\StartSchemaExtension' {directoryId} -> directoryId) (\s@StartSchemaExtension' {} a -> s {directoryId = a} :: StartSchemaExtension)

-- | If true, creates a snapshot of the directory before applying the schema
-- extension.
startSchemaExtension_createSnapshotBeforeSchemaExtension :: Lens.Lens' StartSchemaExtension Prelude.Bool
startSchemaExtension_createSnapshotBeforeSchemaExtension = Lens.lens (\StartSchemaExtension' {createSnapshotBeforeSchemaExtension} -> createSnapshotBeforeSchemaExtension) (\s@StartSchemaExtension' {} a -> s {createSnapshotBeforeSchemaExtension = a} :: StartSchemaExtension)

-- | The LDIF file represented as a string. To construct the LdifContent
-- string, precede each line as it would be formatted in an ldif file with
-- \\n. See the example request below for more details. The file size can
-- be no larger than 1MB.
startSchemaExtension_ldifContent :: Lens.Lens' StartSchemaExtension Prelude.Text
startSchemaExtension_ldifContent = Lens.lens (\StartSchemaExtension' {ldifContent} -> ldifContent) (\s@StartSchemaExtension' {} a -> s {ldifContent = a} :: StartSchemaExtension)

-- | A description of the schema extension.
startSchemaExtension_description :: Lens.Lens' StartSchemaExtension Prelude.Text
startSchemaExtension_description = Lens.lens (\StartSchemaExtension' {description} -> description) (\s@StartSchemaExtension' {} a -> s {description = a} :: StartSchemaExtension)

instance Core.AWSRequest StartSchemaExtension where
  type
    AWSResponse StartSchemaExtension =
      StartSchemaExtensionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartSchemaExtensionResponse'
            Prelude.<$> (x Data..?> "SchemaExtensionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartSchemaExtension where
  hashWithSalt _salt StartSchemaExtension' {..} =
    _salt `Prelude.hashWithSalt` directoryId
      `Prelude.hashWithSalt` createSnapshotBeforeSchemaExtension
      `Prelude.hashWithSalt` ldifContent
      `Prelude.hashWithSalt` description

instance Prelude.NFData StartSchemaExtension where
  rnf StartSchemaExtension' {..} =
    Prelude.rnf directoryId
      `Prelude.seq` Prelude.rnf createSnapshotBeforeSchemaExtension
      `Prelude.seq` Prelude.rnf ldifContent
      `Prelude.seq` Prelude.rnf description

instance Data.ToHeaders StartSchemaExtension where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DirectoryService_20150416.StartSchemaExtension" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartSchemaExtension where
  toJSON StartSchemaExtension' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DirectoryId" Data..= directoryId),
            Prelude.Just
              ( "CreateSnapshotBeforeSchemaExtension"
                  Data..= createSnapshotBeforeSchemaExtension
              ),
            Prelude.Just ("LdifContent" Data..= ldifContent),
            Prelude.Just ("Description" Data..= description)
          ]
      )

instance Data.ToPath StartSchemaExtension where
  toPath = Prelude.const "/"

instance Data.ToQuery StartSchemaExtension where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartSchemaExtensionResponse' smart constructor.
data StartSchemaExtensionResponse = StartSchemaExtensionResponse'
  { -- | The identifier of the schema extension that will be applied.
    schemaExtensionId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartSchemaExtensionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaExtensionId', 'startSchemaExtensionResponse_schemaExtensionId' - The identifier of the schema extension that will be applied.
--
-- 'httpStatus', 'startSchemaExtensionResponse_httpStatus' - The response's http status code.
newStartSchemaExtensionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartSchemaExtensionResponse
newStartSchemaExtensionResponse pHttpStatus_ =
  StartSchemaExtensionResponse'
    { schemaExtensionId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of the schema extension that will be applied.
startSchemaExtensionResponse_schemaExtensionId :: Lens.Lens' StartSchemaExtensionResponse (Prelude.Maybe Prelude.Text)
startSchemaExtensionResponse_schemaExtensionId = Lens.lens (\StartSchemaExtensionResponse' {schemaExtensionId} -> schemaExtensionId) (\s@StartSchemaExtensionResponse' {} a -> s {schemaExtensionId = a} :: StartSchemaExtensionResponse)

-- | The response's http status code.
startSchemaExtensionResponse_httpStatus :: Lens.Lens' StartSchemaExtensionResponse Prelude.Int
startSchemaExtensionResponse_httpStatus = Lens.lens (\StartSchemaExtensionResponse' {httpStatus} -> httpStatus) (\s@StartSchemaExtensionResponse' {} a -> s {httpStatus = a} :: StartSchemaExtensionResponse)

instance Prelude.NFData StartSchemaExtensionResponse where
  rnf StartSchemaExtensionResponse' {..} =
    Prelude.rnf schemaExtensionId
      `Prelude.seq` Prelude.rnf httpStatus
