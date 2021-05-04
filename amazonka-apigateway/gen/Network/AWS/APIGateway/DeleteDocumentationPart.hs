{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Deletes an existing documentation part of an API.
--
-- /See:/ 'newDeleteDocumentationPart' smart constructor.
data DeleteDocumentationPart = DeleteDocumentationPart'
  { -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | [Required] The identifier of the to-be-deleted documentation part.
    documentationPartId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'documentationPartId'
  Prelude.Text ->
  DeleteDocumentationPart
newDeleteDocumentationPart
  pRestApiId_
  pDocumentationPartId_ =
    DeleteDocumentationPart'
      { restApiId = pRestApiId_,
        documentationPartId = pDocumentationPartId_
      }

-- | [Required] The string identifier of the associated RestApi.
deleteDocumentationPart_restApiId :: Lens.Lens' DeleteDocumentationPart Prelude.Text
deleteDocumentationPart_restApiId = Lens.lens (\DeleteDocumentationPart' {restApiId} -> restApiId) (\s@DeleteDocumentationPart' {} a -> s {restApiId = a} :: DeleteDocumentationPart)

-- | [Required] The identifier of the to-be-deleted documentation part.
deleteDocumentationPart_documentationPartId :: Lens.Lens' DeleteDocumentationPart Prelude.Text
deleteDocumentationPart_documentationPartId = Lens.lens (\DeleteDocumentationPart' {documentationPartId} -> documentationPartId) (\s@DeleteDocumentationPart' {} a -> s {documentationPartId = a} :: DeleteDocumentationPart)

instance Prelude.AWSRequest DeleteDocumentationPart where
  type
    Rs DeleteDocumentationPart =
      DeleteDocumentationPartResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull
      DeleteDocumentationPartResponse'

instance Prelude.Hashable DeleteDocumentationPart

instance Prelude.NFData DeleteDocumentationPart

instance Prelude.ToHeaders DeleteDocumentationPart where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Prelude.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Prelude.ToPath DeleteDocumentationPart where
  toPath DeleteDocumentationPart' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Prelude.toBS restApiId,
        "/documentation/parts/",
        Prelude.toBS documentationPartId
      ]

instance Prelude.ToQuery DeleteDocumentationPart where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDocumentationPartResponse' smart constructor.
data DeleteDocumentationPartResponse = DeleteDocumentationPartResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteDocumentationPartResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteDocumentationPartResponse ::
  DeleteDocumentationPartResponse
newDeleteDocumentationPartResponse =
  DeleteDocumentationPartResponse'

instance
  Prelude.NFData
    DeleteDocumentationPartResponse
