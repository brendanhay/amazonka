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
-- Module      : Network.AWS.SageMaker.UpdateArtifact
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an artifact.
module Network.AWS.SageMaker.UpdateArtifact
  ( -- * Creating a Request
    UpdateArtifact (..),
    newUpdateArtifact,

    -- * Request Lenses
    updateArtifact_propertiesToRemove,
    updateArtifact_artifactName,
    updateArtifact_properties,
    updateArtifact_artifactArn,

    -- * Destructuring the Response
    UpdateArtifactResponse (..),
    newUpdateArtifactResponse,

    -- * Response Lenses
    updateArtifactResponse_artifactArn,
    updateArtifactResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newUpdateArtifact' smart constructor.
data UpdateArtifact = UpdateArtifact'
  { -- | A list of properties to remove.
    propertiesToRemove :: Core.Maybe [Core.Text],
    -- | The new name for the artifact.
    artifactName :: Core.Maybe Core.Text,
    -- | The new list of properties. Overwrites the current property list.
    properties :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The Amazon Resource Name (ARN) of the artifact to update.
    artifactArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateArtifact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'propertiesToRemove', 'updateArtifact_propertiesToRemove' - A list of properties to remove.
--
-- 'artifactName', 'updateArtifact_artifactName' - The new name for the artifact.
--
-- 'properties', 'updateArtifact_properties' - The new list of properties. Overwrites the current property list.
--
-- 'artifactArn', 'updateArtifact_artifactArn' - The Amazon Resource Name (ARN) of the artifact to update.
newUpdateArtifact ::
  -- | 'artifactArn'
  Core.Text ->
  UpdateArtifact
newUpdateArtifact pArtifactArn_ =
  UpdateArtifact'
    { propertiesToRemove = Core.Nothing,
      artifactName = Core.Nothing,
      properties = Core.Nothing,
      artifactArn = pArtifactArn_
    }

-- | A list of properties to remove.
updateArtifact_propertiesToRemove :: Lens.Lens' UpdateArtifact (Core.Maybe [Core.Text])
updateArtifact_propertiesToRemove = Lens.lens (\UpdateArtifact' {propertiesToRemove} -> propertiesToRemove) (\s@UpdateArtifact' {} a -> s {propertiesToRemove = a} :: UpdateArtifact) Core.. Lens.mapping Lens._Coerce

-- | The new name for the artifact.
updateArtifact_artifactName :: Lens.Lens' UpdateArtifact (Core.Maybe Core.Text)
updateArtifact_artifactName = Lens.lens (\UpdateArtifact' {artifactName} -> artifactName) (\s@UpdateArtifact' {} a -> s {artifactName = a} :: UpdateArtifact)

-- | The new list of properties. Overwrites the current property list.
updateArtifact_properties :: Lens.Lens' UpdateArtifact (Core.Maybe (Core.HashMap Core.Text Core.Text))
updateArtifact_properties = Lens.lens (\UpdateArtifact' {properties} -> properties) (\s@UpdateArtifact' {} a -> s {properties = a} :: UpdateArtifact) Core.. Lens.mapping Lens._Coerce

-- | The Amazon Resource Name (ARN) of the artifact to update.
updateArtifact_artifactArn :: Lens.Lens' UpdateArtifact Core.Text
updateArtifact_artifactArn = Lens.lens (\UpdateArtifact' {artifactArn} -> artifactArn) (\s@UpdateArtifact' {} a -> s {artifactArn = a} :: UpdateArtifact)

instance Core.AWSRequest UpdateArtifact where
  type
    AWSResponse UpdateArtifact =
      UpdateArtifactResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateArtifactResponse'
            Core.<$> (x Core..?> "ArtifactArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateArtifact

instance Core.NFData UpdateArtifact

instance Core.ToHeaders UpdateArtifact where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.UpdateArtifact" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateArtifact where
  toJSON UpdateArtifact' {..} =
    Core.object
      ( Core.catMaybes
          [ ("PropertiesToRemove" Core..=)
              Core.<$> propertiesToRemove,
            ("ArtifactName" Core..=) Core.<$> artifactName,
            ("Properties" Core..=) Core.<$> properties,
            Core.Just ("ArtifactArn" Core..= artifactArn)
          ]
      )

instance Core.ToPath UpdateArtifact where
  toPath = Core.const "/"

instance Core.ToQuery UpdateArtifact where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateArtifactResponse' smart constructor.
data UpdateArtifactResponse = UpdateArtifactResponse'
  { -- | The Amazon Resource Name (ARN) of the artifact.
    artifactArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateArtifactResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'artifactArn', 'updateArtifactResponse_artifactArn' - The Amazon Resource Name (ARN) of the artifact.
--
-- 'httpStatus', 'updateArtifactResponse_httpStatus' - The response's http status code.
newUpdateArtifactResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateArtifactResponse
newUpdateArtifactResponse pHttpStatus_ =
  UpdateArtifactResponse'
    { artifactArn = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the artifact.
updateArtifactResponse_artifactArn :: Lens.Lens' UpdateArtifactResponse (Core.Maybe Core.Text)
updateArtifactResponse_artifactArn = Lens.lens (\UpdateArtifactResponse' {artifactArn} -> artifactArn) (\s@UpdateArtifactResponse' {} a -> s {artifactArn = a} :: UpdateArtifactResponse)

-- | The response's http status code.
updateArtifactResponse_httpStatus :: Lens.Lens' UpdateArtifactResponse Core.Int
updateArtifactResponse_httpStatus = Lens.lens (\UpdateArtifactResponse' {httpStatus} -> httpStatus) (\s@UpdateArtifactResponse' {} a -> s {httpStatus = a} :: UpdateArtifactResponse)

instance Core.NFData UpdateArtifactResponse
