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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newCreateArtifact' smart constructor.
data CreateArtifact = CreateArtifact'
  { metadataProperties :: Core.Maybe MetadataProperties,
    -- | The name of the artifact. Must be unique to your account in an AWS
    -- Region.
    artifactName :: Core.Maybe Core.Text,
    -- | A list of tags to apply to the artifact.
    tags :: Core.Maybe [Tag],
    -- | A list of properties to add to the artifact.
    properties :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The ID, ID type, and URI of the source.
    source :: ArtifactSource,
    -- | The artifact type.
    artifactType :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  CreateArtifact
newCreateArtifact pSource_ pArtifactType_ =
  CreateArtifact'
    { metadataProperties = Core.Nothing,
      artifactName = Core.Nothing,
      tags = Core.Nothing,
      properties = Core.Nothing,
      source = pSource_,
      artifactType = pArtifactType_
    }

-- | Undocumented member.
createArtifact_metadataProperties :: Lens.Lens' CreateArtifact (Core.Maybe MetadataProperties)
createArtifact_metadataProperties = Lens.lens (\CreateArtifact' {metadataProperties} -> metadataProperties) (\s@CreateArtifact' {} a -> s {metadataProperties = a} :: CreateArtifact)

-- | The name of the artifact. Must be unique to your account in an AWS
-- Region.
createArtifact_artifactName :: Lens.Lens' CreateArtifact (Core.Maybe Core.Text)
createArtifact_artifactName = Lens.lens (\CreateArtifact' {artifactName} -> artifactName) (\s@CreateArtifact' {} a -> s {artifactName = a} :: CreateArtifact)

-- | A list of tags to apply to the artifact.
createArtifact_tags :: Lens.Lens' CreateArtifact (Core.Maybe [Tag])
createArtifact_tags = Lens.lens (\CreateArtifact' {tags} -> tags) (\s@CreateArtifact' {} a -> s {tags = a} :: CreateArtifact) Core.. Lens.mapping Lens._Coerce

-- | A list of properties to add to the artifact.
createArtifact_properties :: Lens.Lens' CreateArtifact (Core.Maybe (Core.HashMap Core.Text Core.Text))
createArtifact_properties = Lens.lens (\CreateArtifact' {properties} -> properties) (\s@CreateArtifact' {} a -> s {properties = a} :: CreateArtifact) Core.. Lens.mapping Lens._Coerce

-- | The ID, ID type, and URI of the source.
createArtifact_source :: Lens.Lens' CreateArtifact ArtifactSource
createArtifact_source = Lens.lens (\CreateArtifact' {source} -> source) (\s@CreateArtifact' {} a -> s {source = a} :: CreateArtifact)

-- | The artifact type.
createArtifact_artifactType :: Lens.Lens' CreateArtifact Core.Text
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
            Core.<$> (x Core..?> "ArtifactArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateArtifact

instance Core.NFData CreateArtifact

instance Core.ToHeaders CreateArtifact where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.CreateArtifact" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateArtifact where
  toJSON CreateArtifact' {..} =
    Core.object
      ( Core.catMaybes
          [ ("MetadataProperties" Core..=)
              Core.<$> metadataProperties,
            ("ArtifactName" Core..=) Core.<$> artifactName,
            ("Tags" Core..=) Core.<$> tags,
            ("Properties" Core..=) Core.<$> properties,
            Core.Just ("Source" Core..= source),
            Core.Just ("ArtifactType" Core..= artifactType)
          ]
      )

instance Core.ToPath CreateArtifact where
  toPath = Core.const "/"

instance Core.ToQuery CreateArtifact where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateArtifactResponse' smart constructor.
data CreateArtifactResponse = CreateArtifactResponse'
  { -- | The Amazon Resource Name (ARN) of the artifact.
    artifactArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  CreateArtifactResponse
newCreateArtifactResponse pHttpStatus_ =
  CreateArtifactResponse'
    { artifactArn = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the artifact.
createArtifactResponse_artifactArn :: Lens.Lens' CreateArtifactResponse (Core.Maybe Core.Text)
createArtifactResponse_artifactArn = Lens.lens (\CreateArtifactResponse' {artifactArn} -> artifactArn) (\s@CreateArtifactResponse' {} a -> s {artifactArn = a} :: CreateArtifactResponse)

-- | The response's http status code.
createArtifactResponse_httpStatus :: Lens.Lens' CreateArtifactResponse Core.Int
createArtifactResponse_httpStatus = Lens.lens (\CreateArtifactResponse' {httpStatus} -> httpStatus) (\s@CreateArtifactResponse' {} a -> s {httpStatus = a} :: CreateArtifactResponse)

instance Core.NFData CreateArtifactResponse
