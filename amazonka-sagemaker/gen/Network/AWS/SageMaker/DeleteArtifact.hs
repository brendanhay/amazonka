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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDeleteArtifact' smart constructor.
data DeleteArtifact = DeleteArtifact'
  { -- | The Amazon Resource Name (ARN) of the artifact to delete.
    artifactArn :: Core.Maybe Core.Text,
    -- | The URI of the source.
    source :: Core.Maybe ArtifactSource
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { artifactArn = Core.Nothing,
      source = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the artifact to delete.
deleteArtifact_artifactArn :: Lens.Lens' DeleteArtifact (Core.Maybe Core.Text)
deleteArtifact_artifactArn = Lens.lens (\DeleteArtifact' {artifactArn} -> artifactArn) (\s@DeleteArtifact' {} a -> s {artifactArn = a} :: DeleteArtifact)

-- | The URI of the source.
deleteArtifact_source :: Lens.Lens' DeleteArtifact (Core.Maybe ArtifactSource)
deleteArtifact_source = Lens.lens (\DeleteArtifact' {source} -> source) (\s@DeleteArtifact' {} a -> s {source = a} :: DeleteArtifact)

instance Core.AWSRequest DeleteArtifact where
  type
    AWSResponse DeleteArtifact =
      DeleteArtifactResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteArtifactResponse'
            Core.<$> (x Core..?> "ArtifactArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteArtifact

instance Core.NFData DeleteArtifact

instance Core.ToHeaders DeleteArtifact where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.DeleteArtifact" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteArtifact where
  toJSON DeleteArtifact' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ArtifactArn" Core..=) Core.<$> artifactArn,
            ("Source" Core..=) Core.<$> source
          ]
      )

instance Core.ToPath DeleteArtifact where
  toPath = Core.const "/"

instance Core.ToQuery DeleteArtifact where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteArtifactResponse' smart constructor.
data DeleteArtifactResponse = DeleteArtifactResponse'
  { -- | The Amazon Resource Name (ARN) of the artifact.
    artifactArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DeleteArtifactResponse
newDeleteArtifactResponse pHttpStatus_ =
  DeleteArtifactResponse'
    { artifactArn = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the artifact.
deleteArtifactResponse_artifactArn :: Lens.Lens' DeleteArtifactResponse (Core.Maybe Core.Text)
deleteArtifactResponse_artifactArn = Lens.lens (\DeleteArtifactResponse' {artifactArn} -> artifactArn) (\s@DeleteArtifactResponse' {} a -> s {artifactArn = a} :: DeleteArtifactResponse)

-- | The response's http status code.
deleteArtifactResponse_httpStatus :: Lens.Lens' DeleteArtifactResponse Core.Int
deleteArtifactResponse_httpStatus = Lens.lens (\DeleteArtifactResponse' {httpStatus} -> httpStatus) (\s@DeleteArtifactResponse' {} a -> s {httpStatus = a} :: DeleteArtifactResponse)

instance Core.NFData DeleteArtifactResponse
