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
-- Module      : Network.AWS.SageMaker.CreateArtifact
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.SageMaker.CreateArtifact
  ( -- * Creating a Request
    CreateArtifact (..),
    newCreateArtifact,

    -- * Request Lenses
    createArtifact_metadataProperties,
    createArtifact_artifactName,
    createArtifact_tags,
    createArtifact_properties,
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newCreateArtifact' smart constructor.
data CreateArtifact = CreateArtifact'
  { metadataProperties :: Prelude.Maybe MetadataProperties,
    -- | The name of the artifact. Must be unique to your account in an AWS
    -- Region.
    artifactName :: Prelude.Maybe Prelude.Text,
    -- | A list of tags to apply to the artifact.
    tags :: Prelude.Maybe [Tag],
    -- | A list of properties to add to the artifact.
    properties :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
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
-- 'metadataProperties', 'createArtifact_metadataProperties' - Undocumented member.
--
-- 'artifactName', 'createArtifact_artifactName' - The name of the artifact. Must be unique to your account in an AWS
-- Region.
--
-- 'tags', 'createArtifact_tags' - A list of tags to apply to the artifact.
--
-- 'properties', 'createArtifact_properties' - A list of properties to add to the artifact.
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
    { metadataProperties =
        Prelude.Nothing,
      artifactName = Prelude.Nothing,
      tags = Prelude.Nothing,
      properties = Prelude.Nothing,
      source = pSource_,
      artifactType = pArtifactType_
    }

-- | Undocumented member.
createArtifact_metadataProperties :: Lens.Lens' CreateArtifact (Prelude.Maybe MetadataProperties)
createArtifact_metadataProperties = Lens.lens (\CreateArtifact' {metadataProperties} -> metadataProperties) (\s@CreateArtifact' {} a -> s {metadataProperties = a} :: CreateArtifact)

-- | The name of the artifact. Must be unique to your account in an AWS
-- Region.
createArtifact_artifactName :: Lens.Lens' CreateArtifact (Prelude.Maybe Prelude.Text)
createArtifact_artifactName = Lens.lens (\CreateArtifact' {artifactName} -> artifactName) (\s@CreateArtifact' {} a -> s {artifactName = a} :: CreateArtifact)

-- | A list of tags to apply to the artifact.
createArtifact_tags :: Lens.Lens' CreateArtifact (Prelude.Maybe [Tag])
createArtifact_tags = Lens.lens (\CreateArtifact' {tags} -> tags) (\s@CreateArtifact' {} a -> s {tags = a} :: CreateArtifact) Prelude.. Lens.mapping Lens._Coerce

-- | A list of properties to add to the artifact.
createArtifact_properties :: Lens.Lens' CreateArtifact (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createArtifact_properties = Lens.lens (\CreateArtifact' {properties} -> properties) (\s@CreateArtifact' {} a -> s {properties = a} :: CreateArtifact) Prelude.. Lens.mapping Lens._Coerce

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateArtifactResponse'
            Prelude.<$> (x Core..?> "ArtifactArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateArtifact

instance Prelude.NFData CreateArtifact

instance Core.ToHeaders CreateArtifact where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.CreateArtifact" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateArtifact where
  toJSON CreateArtifact' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("MetadataProperties" Core..=)
              Prelude.<$> metadataProperties,
            ("ArtifactName" Core..=) Prelude.<$> artifactName,
            ("Tags" Core..=) Prelude.<$> tags,
            ("Properties" Core..=) Prelude.<$> properties,
            Prelude.Just ("Source" Core..= source),
            Prelude.Just ("ArtifactType" Core..= artifactType)
          ]
      )

instance Core.ToPath CreateArtifact where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateArtifact where
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

instance Prelude.NFData CreateArtifactResponse
