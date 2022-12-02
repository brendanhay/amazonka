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
-- Module      : Amazonka.SageMaker.UpdateArtifact
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an artifact.
module Amazonka.SageMaker.UpdateArtifact
  ( -- * Creating a Request
    UpdateArtifact (..),
    newUpdateArtifact,

    -- * Request Lenses
    updateArtifact_artifactName,
    updateArtifact_propertiesToRemove,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newUpdateArtifact' smart constructor.
data UpdateArtifact = UpdateArtifact'
  { -- | The new name for the artifact.
    artifactName :: Prelude.Maybe Prelude.Text,
    -- | A list of properties to remove.
    propertiesToRemove :: Prelude.Maybe [Prelude.Text],
    -- | The new list of properties. Overwrites the current property list.
    properties :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the artifact to update.
    artifactArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateArtifact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'artifactName', 'updateArtifact_artifactName' - The new name for the artifact.
--
-- 'propertiesToRemove', 'updateArtifact_propertiesToRemove' - A list of properties to remove.
--
-- 'properties', 'updateArtifact_properties' - The new list of properties. Overwrites the current property list.
--
-- 'artifactArn', 'updateArtifact_artifactArn' - The Amazon Resource Name (ARN) of the artifact to update.
newUpdateArtifact ::
  -- | 'artifactArn'
  Prelude.Text ->
  UpdateArtifact
newUpdateArtifact pArtifactArn_ =
  UpdateArtifact'
    { artifactName = Prelude.Nothing,
      propertiesToRemove = Prelude.Nothing,
      properties = Prelude.Nothing,
      artifactArn = pArtifactArn_
    }

-- | The new name for the artifact.
updateArtifact_artifactName :: Lens.Lens' UpdateArtifact (Prelude.Maybe Prelude.Text)
updateArtifact_artifactName = Lens.lens (\UpdateArtifact' {artifactName} -> artifactName) (\s@UpdateArtifact' {} a -> s {artifactName = a} :: UpdateArtifact)

-- | A list of properties to remove.
updateArtifact_propertiesToRemove :: Lens.Lens' UpdateArtifact (Prelude.Maybe [Prelude.Text])
updateArtifact_propertiesToRemove = Lens.lens (\UpdateArtifact' {propertiesToRemove} -> propertiesToRemove) (\s@UpdateArtifact' {} a -> s {propertiesToRemove = a} :: UpdateArtifact) Prelude.. Lens.mapping Lens.coerced

-- | The new list of properties. Overwrites the current property list.
updateArtifact_properties :: Lens.Lens' UpdateArtifact (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateArtifact_properties = Lens.lens (\UpdateArtifact' {properties} -> properties) (\s@UpdateArtifact' {} a -> s {properties = a} :: UpdateArtifact) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the artifact to update.
updateArtifact_artifactArn :: Lens.Lens' UpdateArtifact Prelude.Text
updateArtifact_artifactArn = Lens.lens (\UpdateArtifact' {artifactArn} -> artifactArn) (\s@UpdateArtifact' {} a -> s {artifactArn = a} :: UpdateArtifact)

instance Core.AWSRequest UpdateArtifact where
  type
    AWSResponse UpdateArtifact =
      UpdateArtifactResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateArtifactResponse'
            Prelude.<$> (x Data..?> "ArtifactArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateArtifact where
  hashWithSalt _salt UpdateArtifact' {..} =
    _salt `Prelude.hashWithSalt` artifactName
      `Prelude.hashWithSalt` propertiesToRemove
      `Prelude.hashWithSalt` properties
      `Prelude.hashWithSalt` artifactArn

instance Prelude.NFData UpdateArtifact where
  rnf UpdateArtifact' {..} =
    Prelude.rnf artifactName
      `Prelude.seq` Prelude.rnf propertiesToRemove
      `Prelude.seq` Prelude.rnf properties
      `Prelude.seq` Prelude.rnf artifactArn

instance Data.ToHeaders UpdateArtifact where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.UpdateArtifact" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateArtifact where
  toJSON UpdateArtifact' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ArtifactName" Data..=) Prelude.<$> artifactName,
            ("PropertiesToRemove" Data..=)
              Prelude.<$> propertiesToRemove,
            ("Properties" Data..=) Prelude.<$> properties,
            Prelude.Just ("ArtifactArn" Data..= artifactArn)
          ]
      )

instance Data.ToPath UpdateArtifact where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateArtifact where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateArtifactResponse' smart constructor.
data UpdateArtifactResponse = UpdateArtifactResponse'
  { -- | The Amazon Resource Name (ARN) of the artifact.
    artifactArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  UpdateArtifactResponse
newUpdateArtifactResponse pHttpStatus_ =
  UpdateArtifactResponse'
    { artifactArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the artifact.
updateArtifactResponse_artifactArn :: Lens.Lens' UpdateArtifactResponse (Prelude.Maybe Prelude.Text)
updateArtifactResponse_artifactArn = Lens.lens (\UpdateArtifactResponse' {artifactArn} -> artifactArn) (\s@UpdateArtifactResponse' {} a -> s {artifactArn = a} :: UpdateArtifactResponse)

-- | The response's http status code.
updateArtifactResponse_httpStatus :: Lens.Lens' UpdateArtifactResponse Prelude.Int
updateArtifactResponse_httpStatus = Lens.lens (\UpdateArtifactResponse' {httpStatus} -> httpStatus) (\s@UpdateArtifactResponse' {} a -> s {httpStatus = a} :: UpdateArtifactResponse)

instance Prelude.NFData UpdateArtifactResponse where
  rnf UpdateArtifactResponse' {..} =
    Prelude.rnf artifactArn
      `Prelude.seq` Prelude.rnf httpStatus
