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
-- Module      : Amazonka.SageMaker.DeleteArtifact
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an artifact. Either @ArtifactArn@ or @Source@ must be specified.
module Amazonka.SageMaker.DeleteArtifact
  ( -- * Creating a Request
    DeleteArtifact (..),
    newDeleteArtifact,

    -- * Request Lenses
    deleteArtifact_artifactArn,
    deleteArtifact_source,

    -- * Destructuring the Response
    DeleteArtifactResponse (..),
    newDeleteArtifactResponse,

    -- * Response Lenses
    deleteArtifactResponse_artifactArn,
    deleteArtifactResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDeleteArtifact' smart constructor.
data DeleteArtifact = DeleteArtifact'
  { -- | The Amazon Resource Name (ARN) of the artifact to delete.
    artifactArn :: Prelude.Maybe Prelude.Text,
    -- | The URI of the source.
    source :: Prelude.Maybe ArtifactSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteArtifact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'artifactArn', 'deleteArtifact_artifactArn' - The Amazon Resource Name (ARN) of the artifact to delete.
--
-- 'source', 'deleteArtifact_source' - The URI of the source.
newDeleteArtifact ::
  DeleteArtifact
newDeleteArtifact =
  DeleteArtifact'
    { artifactArn = Prelude.Nothing,
      source = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the artifact to delete.
deleteArtifact_artifactArn :: Lens.Lens' DeleteArtifact (Prelude.Maybe Prelude.Text)
deleteArtifact_artifactArn = Lens.lens (\DeleteArtifact' {artifactArn} -> artifactArn) (\s@DeleteArtifact' {} a -> s {artifactArn = a} :: DeleteArtifact)

-- | The URI of the source.
deleteArtifact_source :: Lens.Lens' DeleteArtifact (Prelude.Maybe ArtifactSource)
deleteArtifact_source = Lens.lens (\DeleteArtifact' {source} -> source) (\s@DeleteArtifact' {} a -> s {source = a} :: DeleteArtifact)

instance Core.AWSRequest DeleteArtifact where
  type
    AWSResponse DeleteArtifact =
      DeleteArtifactResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteArtifactResponse'
            Prelude.<$> (x Data..?> "ArtifactArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteArtifact where
  hashWithSalt _salt DeleteArtifact' {..} =
    _salt
      `Prelude.hashWithSalt` artifactArn
      `Prelude.hashWithSalt` source

instance Prelude.NFData DeleteArtifact where
  rnf DeleteArtifact' {..} =
    Prelude.rnf artifactArn
      `Prelude.seq` Prelude.rnf source

instance Data.ToHeaders DeleteArtifact where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.DeleteArtifact" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteArtifact where
  toJSON DeleteArtifact' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ArtifactArn" Data..=) Prelude.<$> artifactArn,
            ("Source" Data..=) Prelude.<$> source
          ]
      )

instance Data.ToPath DeleteArtifact where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteArtifact where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteArtifactResponse' smart constructor.
data DeleteArtifactResponse = DeleteArtifactResponse'
  { -- | The Amazon Resource Name (ARN) of the artifact.
    artifactArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteArtifactResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'artifactArn', 'deleteArtifactResponse_artifactArn' - The Amazon Resource Name (ARN) of the artifact.
--
-- 'httpStatus', 'deleteArtifactResponse_httpStatus' - The response's http status code.
newDeleteArtifactResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteArtifactResponse
newDeleteArtifactResponse pHttpStatus_ =
  DeleteArtifactResponse'
    { artifactArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the artifact.
deleteArtifactResponse_artifactArn :: Lens.Lens' DeleteArtifactResponse (Prelude.Maybe Prelude.Text)
deleteArtifactResponse_artifactArn = Lens.lens (\DeleteArtifactResponse' {artifactArn} -> artifactArn) (\s@DeleteArtifactResponse' {} a -> s {artifactArn = a} :: DeleteArtifactResponse)

-- | The response's http status code.
deleteArtifactResponse_httpStatus :: Lens.Lens' DeleteArtifactResponse Prelude.Int
deleteArtifactResponse_httpStatus = Lens.lens (\DeleteArtifactResponse' {httpStatus} -> httpStatus) (\s@DeleteArtifactResponse' {} a -> s {httpStatus = a} :: DeleteArtifactResponse)

instance Prelude.NFData DeleteArtifactResponse where
  rnf DeleteArtifactResponse' {..} =
    Prelude.rnf artifactArn
      `Prelude.seq` Prelude.rnf httpStatus
