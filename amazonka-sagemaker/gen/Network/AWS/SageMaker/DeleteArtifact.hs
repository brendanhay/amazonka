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
-- Module      : Network.AWS.SageMaker.DeleteArtifact
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an artifact. Either @ArtifactArn@ or @Source@ must be specified.
module Network.AWS.SageMaker.DeleteArtifact
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDeleteArtifact' smart constructor.
data DeleteArtifact = DeleteArtifact'
  { -- | The Amazon Resource Name (ARN) of the artifact to delete.
    artifactArn :: Prelude.Maybe Prelude.Text,
    -- | The URI of the source.
    source :: Prelude.Maybe ArtifactSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DeleteArtifact where
  type Rs DeleteArtifact = DeleteArtifactResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteArtifactResponse'
            Prelude.<$> (x Prelude..?> "ArtifactArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteArtifact

instance Prelude.NFData DeleteArtifact

instance Prelude.ToHeaders DeleteArtifact where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("SageMaker.DeleteArtifact" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteArtifact where
  toJSON DeleteArtifact' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ArtifactArn" Prelude..=) Prelude.<$> artifactArn,
            ("Source" Prelude..=) Prelude.<$> source
          ]
      )

instance Prelude.ToPath DeleteArtifact where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteArtifact where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteArtifactResponse' smart constructor.
data DeleteArtifactResponse = DeleteArtifactResponse'
  { -- | The Amazon Resource Name (ARN) of the artifact.
    artifactArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData DeleteArtifactResponse
