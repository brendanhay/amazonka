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
-- Module      : Amazonka.APIGateway.DeleteDocumentationVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a documentation version.
module Amazonka.APIGateway.DeleteDocumentationVersion
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

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Deletes an existing documentation version of an API.
--
-- /See:/ 'newDeleteDocumentationVersion' smart constructor.
data DeleteDocumentationVersion = DeleteDocumentationVersion'
  { -- | The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | The version identifier of a to-be-deleted documentation snapshot.
    documentationVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDocumentationVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restApiId', 'deleteDocumentationVersion_restApiId' - The string identifier of the associated RestApi.
--
-- 'documentationVersion', 'deleteDocumentationVersion_documentationVersion' - The version identifier of a to-be-deleted documentation snapshot.
newDeleteDocumentationVersion ::
  -- | 'restApiId'
  Prelude.Text ->
  -- | 'documentationVersion'
  Prelude.Text ->
  DeleteDocumentationVersion
newDeleteDocumentationVersion
  pRestApiId_
  pDocumentationVersion_ =
    DeleteDocumentationVersion'
      { restApiId =
          pRestApiId_,
        documentationVersion = pDocumentationVersion_
      }

-- | The string identifier of the associated RestApi.
deleteDocumentationVersion_restApiId :: Lens.Lens' DeleteDocumentationVersion Prelude.Text
deleteDocumentationVersion_restApiId = Lens.lens (\DeleteDocumentationVersion' {restApiId} -> restApiId) (\s@DeleteDocumentationVersion' {} a -> s {restApiId = a} :: DeleteDocumentationVersion)

-- | The version identifier of a to-be-deleted documentation snapshot.
deleteDocumentationVersion_documentationVersion :: Lens.Lens' DeleteDocumentationVersion Prelude.Text
deleteDocumentationVersion_documentationVersion = Lens.lens (\DeleteDocumentationVersion' {documentationVersion} -> documentationVersion) (\s@DeleteDocumentationVersion' {} a -> s {documentationVersion = a} :: DeleteDocumentationVersion)

instance Core.AWSRequest DeleteDocumentationVersion where
  type
    AWSResponse DeleteDocumentationVersion =
      DeleteDocumentationVersionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DeleteDocumentationVersionResponse'

instance Prelude.Hashable DeleteDocumentationVersion where
  hashWithSalt _salt DeleteDocumentationVersion' {..} =
    _salt
      `Prelude.hashWithSalt` restApiId
      `Prelude.hashWithSalt` documentationVersion

instance Prelude.NFData DeleteDocumentationVersion where
  rnf DeleteDocumentationVersion' {..} =
    Prelude.rnf restApiId `Prelude.seq`
      Prelude.rnf documentationVersion

instance Data.ToHeaders DeleteDocumentationVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToPath DeleteDocumentationVersion where
  toPath DeleteDocumentationVersion' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Data.toBS restApiId,
        "/documentation/versions/",
        Data.toBS documentationVersion
      ]

instance Data.ToQuery DeleteDocumentationVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDocumentationVersionResponse' smart constructor.
data DeleteDocumentationVersionResponse = DeleteDocumentationVersionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDocumentationVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteDocumentationVersionResponse ::
  DeleteDocumentationVersionResponse
newDeleteDocumentationVersionResponse =
  DeleteDocumentationVersionResponse'

instance
  Prelude.NFData
    DeleteDocumentationVersionResponse
  where
  rnf _ = ()
