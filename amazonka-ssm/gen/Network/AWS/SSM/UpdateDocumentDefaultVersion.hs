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
-- Module      : Network.AWS.SSM.UpdateDocumentDefaultVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Set the default version of a document.
module Network.AWS.SSM.UpdateDocumentDefaultVersion
  ( -- * Creating a Request
    UpdateDocumentDefaultVersion (..),
    newUpdateDocumentDefaultVersion,

    -- * Request Lenses
    updateDocumentDefaultVersion_name,
    updateDocumentDefaultVersion_documentVersion,

    -- * Destructuring the Response
    UpdateDocumentDefaultVersionResponse (..),
    newUpdateDocumentDefaultVersionResponse,

    -- * Response Lenses
    updateDocumentDefaultVersionResponse_description,
    updateDocumentDefaultVersionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newUpdateDocumentDefaultVersion' smart constructor.
data UpdateDocumentDefaultVersion = UpdateDocumentDefaultVersion'
  { -- | The name of a custom document that you want to set as the default
    -- version.
    name :: Core.Text,
    -- | The version of a custom document that you want to set as the default
    -- version.
    documentVersion :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateDocumentDefaultVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateDocumentDefaultVersion_name' - The name of a custom document that you want to set as the default
-- version.
--
-- 'documentVersion', 'updateDocumentDefaultVersion_documentVersion' - The version of a custom document that you want to set as the default
-- version.
newUpdateDocumentDefaultVersion ::
  -- | 'name'
  Core.Text ->
  -- | 'documentVersion'
  Core.Text ->
  UpdateDocumentDefaultVersion
newUpdateDocumentDefaultVersion
  pName_
  pDocumentVersion_ =
    UpdateDocumentDefaultVersion'
      { name = pName_,
        documentVersion = pDocumentVersion_
      }

-- | The name of a custom document that you want to set as the default
-- version.
updateDocumentDefaultVersion_name :: Lens.Lens' UpdateDocumentDefaultVersion Core.Text
updateDocumentDefaultVersion_name = Lens.lens (\UpdateDocumentDefaultVersion' {name} -> name) (\s@UpdateDocumentDefaultVersion' {} a -> s {name = a} :: UpdateDocumentDefaultVersion)

-- | The version of a custom document that you want to set as the default
-- version.
updateDocumentDefaultVersion_documentVersion :: Lens.Lens' UpdateDocumentDefaultVersion Core.Text
updateDocumentDefaultVersion_documentVersion = Lens.lens (\UpdateDocumentDefaultVersion' {documentVersion} -> documentVersion) (\s@UpdateDocumentDefaultVersion' {} a -> s {documentVersion = a} :: UpdateDocumentDefaultVersion)

instance Core.AWSRequest UpdateDocumentDefaultVersion where
  type
    AWSResponse UpdateDocumentDefaultVersion =
      UpdateDocumentDefaultVersionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDocumentDefaultVersionResponse'
            Core.<$> (x Core..?> "Description")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateDocumentDefaultVersion

instance Core.NFData UpdateDocumentDefaultVersion

instance Core.ToHeaders UpdateDocumentDefaultVersion where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.UpdateDocumentDefaultVersion" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateDocumentDefaultVersion where
  toJSON UpdateDocumentDefaultVersion' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just
              ("DocumentVersion" Core..= documentVersion)
          ]
      )

instance Core.ToPath UpdateDocumentDefaultVersion where
  toPath = Core.const "/"

instance Core.ToQuery UpdateDocumentDefaultVersion where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateDocumentDefaultVersionResponse' smart constructor.
data UpdateDocumentDefaultVersionResponse = UpdateDocumentDefaultVersionResponse'
  { -- | The description of a custom document that you want to set as the default
    -- version.
    description :: Core.Maybe DocumentDefaultVersionDescription,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateDocumentDefaultVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateDocumentDefaultVersionResponse_description' - The description of a custom document that you want to set as the default
-- version.
--
-- 'httpStatus', 'updateDocumentDefaultVersionResponse_httpStatus' - The response's http status code.
newUpdateDocumentDefaultVersionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateDocumentDefaultVersionResponse
newUpdateDocumentDefaultVersionResponse pHttpStatus_ =
  UpdateDocumentDefaultVersionResponse'
    { description =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The description of a custom document that you want to set as the default
-- version.
updateDocumentDefaultVersionResponse_description :: Lens.Lens' UpdateDocumentDefaultVersionResponse (Core.Maybe DocumentDefaultVersionDescription)
updateDocumentDefaultVersionResponse_description = Lens.lens (\UpdateDocumentDefaultVersionResponse' {description} -> description) (\s@UpdateDocumentDefaultVersionResponse' {} a -> s {description = a} :: UpdateDocumentDefaultVersionResponse)

-- | The response's http status code.
updateDocumentDefaultVersionResponse_httpStatus :: Lens.Lens' UpdateDocumentDefaultVersionResponse Core.Int
updateDocumentDefaultVersionResponse_httpStatus = Lens.lens (\UpdateDocumentDefaultVersionResponse' {httpStatus} -> httpStatus) (\s@UpdateDocumentDefaultVersionResponse' {} a -> s {httpStatus = a} :: UpdateDocumentDefaultVersionResponse)

instance
  Core.NFData
    UpdateDocumentDefaultVersionResponse
