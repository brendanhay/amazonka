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
-- Module      : Network.AWS.DirectoryService.StartSchemaExtension
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Applies a schema extension to a Microsoft AD directory.
module Network.AWS.DirectoryService.StartSchemaExtension
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

import qualified Network.AWS.Core as Core
import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartSchemaExtension' smart constructor.
data StartSchemaExtension = StartSchemaExtension'
  { -- | The identifier of the directory for which the schema extension will be
    -- applied to.
    directoryId :: Core.Text,
    -- | If true, creates a snapshot of the directory before applying the schema
    -- extension.
    createSnapshotBeforeSchemaExtension :: Core.Bool,
    -- | The LDIF file represented as a string. To construct the LdifContent
    -- string, precede each line as it would be formatted in an ldif file with
    -- \\n. See the example request below for more details. The file size can
    -- be no larger than 1MB.
    ldifContent :: Core.Text,
    -- | A description of the schema extension.
    description :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'createSnapshotBeforeSchemaExtension'
  Core.Bool ->
  -- | 'ldifContent'
  Core.Text ->
  -- | 'description'
  Core.Text ->
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
startSchemaExtension_directoryId :: Lens.Lens' StartSchemaExtension Core.Text
startSchemaExtension_directoryId = Lens.lens (\StartSchemaExtension' {directoryId} -> directoryId) (\s@StartSchemaExtension' {} a -> s {directoryId = a} :: StartSchemaExtension)

-- | If true, creates a snapshot of the directory before applying the schema
-- extension.
startSchemaExtension_createSnapshotBeforeSchemaExtension :: Lens.Lens' StartSchemaExtension Core.Bool
startSchemaExtension_createSnapshotBeforeSchemaExtension = Lens.lens (\StartSchemaExtension' {createSnapshotBeforeSchemaExtension} -> createSnapshotBeforeSchemaExtension) (\s@StartSchemaExtension' {} a -> s {createSnapshotBeforeSchemaExtension = a} :: StartSchemaExtension)

-- | The LDIF file represented as a string. To construct the LdifContent
-- string, precede each line as it would be formatted in an ldif file with
-- \\n. See the example request below for more details. The file size can
-- be no larger than 1MB.
startSchemaExtension_ldifContent :: Lens.Lens' StartSchemaExtension Core.Text
startSchemaExtension_ldifContent = Lens.lens (\StartSchemaExtension' {ldifContent} -> ldifContent) (\s@StartSchemaExtension' {} a -> s {ldifContent = a} :: StartSchemaExtension)

-- | A description of the schema extension.
startSchemaExtension_description :: Lens.Lens' StartSchemaExtension Core.Text
startSchemaExtension_description = Lens.lens (\StartSchemaExtension' {description} -> description) (\s@StartSchemaExtension' {} a -> s {description = a} :: StartSchemaExtension)

instance Core.AWSRequest StartSchemaExtension where
  type
    AWSResponse StartSchemaExtension =
      StartSchemaExtensionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartSchemaExtensionResponse'
            Core.<$> (x Core..?> "SchemaExtensionId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StartSchemaExtension

instance Core.NFData StartSchemaExtension

instance Core.ToHeaders StartSchemaExtension where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DirectoryService_20150416.StartSchemaExtension" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StartSchemaExtension where
  toJSON StartSchemaExtension' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DirectoryId" Core..= directoryId),
            Core.Just
              ( "CreateSnapshotBeforeSchemaExtension"
                  Core..= createSnapshotBeforeSchemaExtension
              ),
            Core.Just ("LdifContent" Core..= ldifContent),
            Core.Just ("Description" Core..= description)
          ]
      )

instance Core.ToPath StartSchemaExtension where
  toPath = Core.const "/"

instance Core.ToQuery StartSchemaExtension where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStartSchemaExtensionResponse' smart constructor.
data StartSchemaExtensionResponse = StartSchemaExtensionResponse'
  { -- | The identifier of the schema extension that will be applied.
    schemaExtensionId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  StartSchemaExtensionResponse
newStartSchemaExtensionResponse pHttpStatus_ =
  StartSchemaExtensionResponse'
    { schemaExtensionId =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of the schema extension that will be applied.
startSchemaExtensionResponse_schemaExtensionId :: Lens.Lens' StartSchemaExtensionResponse (Core.Maybe Core.Text)
startSchemaExtensionResponse_schemaExtensionId = Lens.lens (\StartSchemaExtensionResponse' {schemaExtensionId} -> schemaExtensionId) (\s@StartSchemaExtensionResponse' {} a -> s {schemaExtensionId = a} :: StartSchemaExtensionResponse)

-- | The response's http status code.
startSchemaExtensionResponse_httpStatus :: Lens.Lens' StartSchemaExtensionResponse Core.Int
startSchemaExtensionResponse_httpStatus = Lens.lens (\StartSchemaExtensionResponse' {httpStatus} -> httpStatus) (\s@StartSchemaExtensionResponse' {} a -> s {httpStatus = a} :: StartSchemaExtensionResponse)

instance Core.NFData StartSchemaExtensionResponse
