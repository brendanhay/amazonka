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
-- Module      : Amazonka.SageMaker.CreateArtifact
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an /artifact/. An artifact is a lineage tracking entity that
-- represents a URI addressable object or data. Some examples are the S3
-- URI of a dataset and the ECR registry path of an image. For more
-- information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/lineage-tracking.html Amazon SageMaker ML Lineage Tracking>.
module Amazonka.SageMaker.CreateArtifact
  ( -- * Creating a Request
    CreateArtifact (..),
    newCreateArtifact,

    -- * Request Lenses
    createArtifact_artifactName,
    createArtifact_metadataProperties,
    createArtifact_properties,
    createArtifact_tags,
    createArtifact_source,
    createArtifact_artifactType,

    -- * Destructuring the Response
    CreateArtifactResponse (..),
    newCreateArtifactResponse,

    -- * Response Lenses
    createArtifactResponse_artifactArn,
    createArtifactResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newCreateArtifact' smart constructor.
data CreateArtifact = CreateArtifact'
  { -- | The name of the artifact. Must be unique to your account in an Amazon
    -- Web Services Region.
    artifactName :: Prelude.Maybe Prelude.Text,
    metadataProperties :: Prelude.Maybe MetadataProperties,
    -- | A list of properties to add to the artifact.
    properties :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A list of tags to apply to the artifact.
    tags :: Prelude.Maybe [Tag],
    -- | The ID, ID type, and URI of the source.
    source :: ArtifactSource,
    -- | The artifact type.
    artifactType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateArtifact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'artifactName', 'createArtifact_artifactName' - The name of the artifact. Must be unique to your account in an Amazon
-- Web Services Region.
--
-- 'metadataProperties', 'createArtifact_metadataProperties' - Undocumented member.
--
-- 'properties', 'createArtifact_properties' - A list of properties to add to the artifact.
--
-- 'tags', 'createArtifact_tags' - A list of tags to apply to the artifact.
--
-- 'source', 'createArtifact_source' - The ID, ID type, and URI of the source.
--
-- 'artifactType', 'createArtifact_artifactType' - The artifact type.
newCreateArtifact ::
  -- | 'source'
  ArtifactSource ->
  -- | 'artifactType'
  Prelude.Text ->
  CreateArtifact
newCreateArtifact pSource_ pArtifactType_ =
  CreateArtifact'
    { artifactName = Prelude.Nothing,
      metadataProperties = Prelude.Nothing,
      properties = Prelude.Nothing,
      tags = Prelude.Nothing,
      source = pSource_,
      artifactType = pArtifactType_
    }

-- | The name of the artifact. Must be unique to your account in an Amazon
-- Web Services Region.
createArtifact_artifactName :: Lens.Lens' CreateArtifact (Prelude.Maybe Prelude.Text)
createArtifact_artifactName = Lens.lens (\CreateArtifact' {artifactName} -> artifactName) (\s@CreateArtifact' {} a -> s {artifactName = a} :: CreateArtifact)

-- | Undocumented member.
createArtifact_metadataProperties :: Lens.Lens' CreateArtifact (Prelude.Maybe MetadataProperties)
createArtifact_metadataProperties = Lens.lens (\CreateArtifact' {metadataProperties} -> metadataProperties) (\s@CreateArtifact' {} a -> s {metadataProperties = a} :: CreateArtifact)

-- | A list of properties to add to the artifact.
createArtifact_properties :: Lens.Lens' CreateArtifact (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createArtifact_properties = Lens.lens (\CreateArtifact' {properties} -> properties) (\s@CreateArtifact' {} a -> s {properties = a} :: CreateArtifact) Prelude.. Lens.mapping Lens.coerced

-- | A list of tags to apply to the artifact.
createArtifact_tags :: Lens.Lens' CreateArtifact (Prelude.Maybe [Tag])
createArtifact_tags = Lens.lens (\CreateArtifact' {tags} -> tags) (\s@CreateArtifact' {} a -> s {tags = a} :: CreateArtifact) Prelude.. Lens.mapping Lens.coerced

-- | The ID, ID type, and URI of the source.
createArtifact_source :: Lens.Lens' CreateArtifact ArtifactSource
createArtifact_source = Lens.lens (\CreateArtifact' {source} -> source) (\s@CreateArtifact' {} a -> s {source = a} :: CreateArtifact)

-- | The artifact type.
createArtifact_artifactType :: Lens.Lens' CreateArtifact Prelude.Text
createArtifact_artifactType = Lens.lens (\CreateArtifact' {artifactType} -> artifactType) (\s@CreateArtifact' {} a -> s {artifactType = a} :: CreateArtifact)

instance Core.AWSRequest CreateArtifact where
  type
    AWSResponse CreateArtifact =
      CreateArtifactResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateArtifactResponse'
            Prelude.<$> (x Data..?> "ArtifactArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateArtifact where
  hashWithSalt _salt CreateArtifact' {..} =
    _salt
      `Prelude.hashWithSalt` artifactName
      `Prelude.hashWithSalt` metadataProperties
      `Prelude.hashWithSalt` properties
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` artifactType

instance Prelude.NFData CreateArtifact where
  rnf CreateArtifact' {..} =
    Prelude.rnf artifactName
      `Prelude.seq` Prelude.rnf metadataProperties
      `Prelude.seq` Prelude.rnf properties
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf artifactType

instance Data.ToHeaders CreateArtifact where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.CreateArtifact" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateArtifact where
  toJSON CreateArtifact' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ArtifactName" Data..=) Prelude.<$> artifactName,
            ("MetadataProperties" Data..=)
              Prelude.<$> metadataProperties,
            ("Properties" Data..=) Prelude.<$> properties,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Source" Data..= source),
            Prelude.Just ("ArtifactType" Data..= artifactType)
          ]
      )

instance Data.ToPath CreateArtifact where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateArtifact where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateArtifactResponse' smart constructor.
data CreateArtifactResponse = CreateArtifactResponse'
  { -- | The Amazon Resource Name (ARN) of the artifact.
    artifactArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateArtifactResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'artifactArn', 'createArtifactResponse_artifactArn' - The Amazon Resource Name (ARN) of the artifact.
--
-- 'httpStatus', 'createArtifactResponse_httpStatus' - The response's http status code.
newCreateArtifactResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateArtifactResponse
newCreateArtifactResponse pHttpStatus_ =
  CreateArtifactResponse'
    { artifactArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the artifact.
createArtifactResponse_artifactArn :: Lens.Lens' CreateArtifactResponse (Prelude.Maybe Prelude.Text)
createArtifactResponse_artifactArn = Lens.lens (\CreateArtifactResponse' {artifactArn} -> artifactArn) (\s@CreateArtifactResponse' {} a -> s {artifactArn = a} :: CreateArtifactResponse)

-- | The response's http status code.
createArtifactResponse_httpStatus :: Lens.Lens' CreateArtifactResponse Prelude.Int
createArtifactResponse_httpStatus = Lens.lens (\CreateArtifactResponse' {httpStatus} -> httpStatus) (\s@CreateArtifactResponse' {} a -> s {httpStatus = a} :: CreateArtifactResponse)

instance Prelude.NFData CreateArtifactResponse where
  rnf CreateArtifactResponse' {..} =
    Prelude.rnf artifactArn
      `Prelude.seq` Prelude.rnf httpStatus
