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
-- Module      : Network.AWS.APIGateway.DeleteDocumentationPart
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- -- | Undocumented operation.
module Network.AWS.APIGateway.DeleteDocumentationPart
  ( -- * Creating a Request
    DeleteDocumentationPart (..),
    newDeleteDocumentationPart,

    -- * Request Lenses
    deleteDocumentationPart_restApiId,
    deleteDocumentationPart_documentationPartId,

    -- * Destructuring the Response
    DeleteDocumentationPartResponse (..),
    newDeleteDocumentationPartResponse,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Deletes an existing documentation part of an API.
--
-- /See:/ 'newDeleteDocumentationPart' smart constructor.
data DeleteDocumentationPart = DeleteDocumentationPart'
  { -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Core.Text,
    -- | [Required] The identifier of the to-be-deleted documentation part.
    documentationPartId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteDocumentationPart' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restApiId', 'deleteDocumentationPart_restApiId' - [Required] The string identifier of the associated RestApi.
--
-- 'documentationPartId', 'deleteDocumentationPart_documentationPartId' - [Required] The identifier of the to-be-deleted documentation part.
newDeleteDocumentationPart ::
  -- | 'restApiId'
  Core.Text ->
  -- | 'documentationPartId'
  Core.Text ->
  DeleteDocumentationPart
newDeleteDocumentationPart
  pRestApiId_
  pDocumentationPartId_ =
    DeleteDocumentationPart'
      { restApiId = pRestApiId_,
        documentationPartId = pDocumentationPartId_
      }

-- | [Required] The string identifier of the associated RestApi.
deleteDocumentationPart_restApiId :: Lens.Lens' DeleteDocumentationPart Core.Text
deleteDocumentationPart_restApiId = Lens.lens (\DeleteDocumentationPart' {restApiId} -> restApiId) (\s@DeleteDocumentationPart' {} a -> s {restApiId = a} :: DeleteDocumentationPart)

-- | [Required] The identifier of the to-be-deleted documentation part.
deleteDocumentationPart_documentationPartId :: Lens.Lens' DeleteDocumentationPart Core.Text
deleteDocumentationPart_documentationPartId = Lens.lens (\DeleteDocumentationPart' {documentationPartId} -> documentationPartId) (\s@DeleteDocumentationPart' {} a -> s {documentationPartId = a} :: DeleteDocumentationPart)

instance Core.AWSRequest DeleteDocumentationPart where
  type
    AWSResponse DeleteDocumentationPart =
      DeleteDocumentationPartResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull
      DeleteDocumentationPartResponse'

instance Core.Hashable DeleteDocumentationPart

instance Core.NFData DeleteDocumentationPart

instance Core.ToHeaders DeleteDocumentationPart where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Core.ByteString)
          ]
      )

instance Core.ToPath DeleteDocumentationPart where
  toPath DeleteDocumentationPart' {..} =
    Core.mconcat
      [ "/restapis/",
        Core.toBS restApiId,
        "/documentation/parts/",
        Core.toBS documentationPartId
      ]

instance Core.ToQuery DeleteDocumentationPart where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteDocumentationPartResponse' smart constructor.
data DeleteDocumentationPartResponse = DeleteDocumentationPartResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteDocumentationPartResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteDocumentationPartResponse ::
  DeleteDocumentationPartResponse
newDeleteDocumentationPartResponse =
  DeleteDocumentationPartResponse'

instance Core.NFData DeleteDocumentationPartResponse
