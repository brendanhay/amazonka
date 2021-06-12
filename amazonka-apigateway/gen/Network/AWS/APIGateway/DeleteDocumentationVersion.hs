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
-- Module      : Network.AWS.APIGateway.DeleteDocumentationVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- -- | Undocumented operation.
module Network.AWS.APIGateway.DeleteDocumentationVersion
  ( -- * Creating a Request
    DeleteDocumentationVersion (..),
    newDeleteDocumentationVersion,

    -- * Request Lenses
    deleteDocumentationVersion_restApiId,
    deleteDocumentationVersion_documentationVersion,

    -- * Destructuring the Response
    DeleteDocumentationVersionResponse (..),
    newDeleteDocumentationVersionResponse,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Deletes an existing documentation version of an API.
--
-- /See:/ 'newDeleteDocumentationVersion' smart constructor.
data DeleteDocumentationVersion = DeleteDocumentationVersion'
  { -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Core.Text,
    -- | [Required] The version identifier of a to-be-deleted documentation
    -- snapshot.
    documentationVersion :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteDocumentationVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restApiId', 'deleteDocumentationVersion_restApiId' - [Required] The string identifier of the associated RestApi.
--
-- 'documentationVersion', 'deleteDocumentationVersion_documentationVersion' - [Required] The version identifier of a to-be-deleted documentation
-- snapshot.
newDeleteDocumentationVersion ::
  -- | 'restApiId'
  Core.Text ->
  -- | 'documentationVersion'
  Core.Text ->
  DeleteDocumentationVersion
newDeleteDocumentationVersion
  pRestApiId_
  pDocumentationVersion_ =
    DeleteDocumentationVersion'
      { restApiId =
          pRestApiId_,
        documentationVersion = pDocumentationVersion_
      }

-- | [Required] The string identifier of the associated RestApi.
deleteDocumentationVersion_restApiId :: Lens.Lens' DeleteDocumentationVersion Core.Text
deleteDocumentationVersion_restApiId = Lens.lens (\DeleteDocumentationVersion' {restApiId} -> restApiId) (\s@DeleteDocumentationVersion' {} a -> s {restApiId = a} :: DeleteDocumentationVersion)

-- | [Required] The version identifier of a to-be-deleted documentation
-- snapshot.
deleteDocumentationVersion_documentationVersion :: Lens.Lens' DeleteDocumentationVersion Core.Text
deleteDocumentationVersion_documentationVersion = Lens.lens (\DeleteDocumentationVersion' {documentationVersion} -> documentationVersion) (\s@DeleteDocumentationVersion' {} a -> s {documentationVersion = a} :: DeleteDocumentationVersion)

instance Core.AWSRequest DeleteDocumentationVersion where
  type
    AWSResponse DeleteDocumentationVersion =
      DeleteDocumentationVersionResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull
      DeleteDocumentationVersionResponse'

instance Core.Hashable DeleteDocumentationVersion

instance Core.NFData DeleteDocumentationVersion

instance Core.ToHeaders DeleteDocumentationVersion where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Core.ByteString)
          ]
      )

instance Core.ToPath DeleteDocumentationVersion where
  toPath DeleteDocumentationVersion' {..} =
    Core.mconcat
      [ "/restapis/",
        Core.toBS restApiId,
        "/documentation/versions/",
        Core.toBS documentationVersion
      ]

instance Core.ToQuery DeleteDocumentationVersion where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteDocumentationVersionResponse' smart constructor.
data DeleteDocumentationVersionResponse = DeleteDocumentationVersionResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteDocumentationVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteDocumentationVersionResponse ::
  DeleteDocumentationVersionResponse
newDeleteDocumentationVersionResponse =
  DeleteDocumentationVersionResponse'

instance
  Core.NFData
    DeleteDocumentationVersionResponse
