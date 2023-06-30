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
-- Module      : Amazonka.APIGateway.DeleteDocumentationPart
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a documentation part
module Amazonka.APIGateway.DeleteDocumentationPart
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

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Deletes an existing documentation part of an API.
--
-- /See:/ 'newDeleteDocumentationPart' smart constructor.
data DeleteDocumentationPart = DeleteDocumentationPart'
  { -- | The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | The identifier of the to-be-deleted documentation part.
    documentationPartId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDocumentationPart' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restApiId', 'deleteDocumentationPart_restApiId' - The string identifier of the associated RestApi.
--
-- 'documentationPartId', 'deleteDocumentationPart_documentationPartId' - The identifier of the to-be-deleted documentation part.
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

-- | The string identifier of the associated RestApi.
deleteDocumentationPart_restApiId :: Lens.Lens' DeleteDocumentationPart Prelude.Text
deleteDocumentationPart_restApiId = Lens.lens (\DeleteDocumentationPart' {restApiId} -> restApiId) (\s@DeleteDocumentationPart' {} a -> s {restApiId = a} :: DeleteDocumentationPart)

-- | The identifier of the to-be-deleted documentation part.
deleteDocumentationPart_documentationPartId :: Lens.Lens' DeleteDocumentationPart Prelude.Text
deleteDocumentationPart_documentationPartId = Lens.lens (\DeleteDocumentationPart' {documentationPartId} -> documentationPartId) (\s@DeleteDocumentationPart' {} a -> s {documentationPartId = a} :: DeleteDocumentationPart)

instance Core.AWSRequest DeleteDocumentationPart where
  type
    AWSResponse DeleteDocumentationPart =
      DeleteDocumentationPartResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DeleteDocumentationPartResponse'

instance Prelude.Hashable DeleteDocumentationPart where
  hashWithSalt _salt DeleteDocumentationPart' {..} =
    _salt
      `Prelude.hashWithSalt` restApiId
      `Prelude.hashWithSalt` documentationPartId

instance Prelude.NFData DeleteDocumentationPart where
  rnf DeleteDocumentationPart' {..} =
    Prelude.rnf restApiId
      `Prelude.seq` Prelude.rnf documentationPartId

instance Data.ToHeaders DeleteDocumentationPart where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToPath DeleteDocumentationPart where
  toPath DeleteDocumentationPart' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Data.toBS restApiId,
        "/documentation/parts/",
        Data.toBS documentationPartId
      ]

instance Data.ToQuery DeleteDocumentationPart where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDocumentationPartResponse' smart constructor.
data DeleteDocumentationPartResponse = DeleteDocumentationPartResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf _ = ()
