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
-- Module      : Network.AWS.SageMaker.DescribeArtifact
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an artifact.
module Network.AWS.SageMaker.DescribeArtifact
  ( -- * Creating a Request
    DescribeArtifact (..),
    newDescribeArtifact,

    -- * Request Lenses
    describeArtifact_artifactArn,

    -- * Destructuring the Response
    DescribeArtifactResponse (..),
    newDescribeArtifactResponse,

    -- * Response Lenses
    describeArtifactResponse_metadataProperties,
    describeArtifactResponse_creationTime,
    describeArtifactResponse_artifactName,
    describeArtifactResponse_artifactType,
    describeArtifactResponse_artifactArn,
    describeArtifactResponse_source,
    describeArtifactResponse_properties,
    describeArtifactResponse_lastModifiedTime,
    describeArtifactResponse_createdBy,
    describeArtifactResponse_lastModifiedBy,
    describeArtifactResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDescribeArtifact' smart constructor.
data DescribeArtifact = DescribeArtifact'
  { -- | The Amazon Resource Name (ARN) of the artifact to describe.
    artifactArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeArtifact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'artifactArn', 'describeArtifact_artifactArn' - The Amazon Resource Name (ARN) of the artifact to describe.
newDescribeArtifact ::
  -- | 'artifactArn'
  Core.Text ->
  DescribeArtifact
newDescribeArtifact pArtifactArn_ =
  DescribeArtifact' {artifactArn = pArtifactArn_}

-- | The Amazon Resource Name (ARN) of the artifact to describe.
describeArtifact_artifactArn :: Lens.Lens' DescribeArtifact Core.Text
describeArtifact_artifactArn = Lens.lens (\DescribeArtifact' {artifactArn} -> artifactArn) (\s@DescribeArtifact' {} a -> s {artifactArn = a} :: DescribeArtifact)

instance Core.AWSRequest DescribeArtifact where
  type
    AWSResponse DescribeArtifact =
      DescribeArtifactResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeArtifactResponse'
            Core.<$> (x Core..?> "MetadataProperties")
            Core.<*> (x Core..?> "CreationTime")
            Core.<*> (x Core..?> "ArtifactName")
            Core.<*> (x Core..?> "ArtifactType")
            Core.<*> (x Core..?> "ArtifactArn")
            Core.<*> (x Core..?> "Source")
            Core.<*> (x Core..?> "Properties" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "LastModifiedTime")
            Core.<*> (x Core..?> "CreatedBy")
            Core.<*> (x Core..?> "LastModifiedBy")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeArtifact

instance Core.NFData DescribeArtifact

instance Core.ToHeaders DescribeArtifact where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.DescribeArtifact" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeArtifact where
  toJSON DescribeArtifact' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("ArtifactArn" Core..= artifactArn)]
      )

instance Core.ToPath DescribeArtifact where
  toPath = Core.const "/"

instance Core.ToQuery DescribeArtifact where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeArtifactResponse' smart constructor.
data DescribeArtifactResponse = DescribeArtifactResponse'
  { metadataProperties :: Core.Maybe MetadataProperties,
    -- | When the artifact was created.
    creationTime :: Core.Maybe Core.POSIX,
    -- | The name of the artifact.
    artifactName :: Core.Maybe Core.Text,
    -- | The type of the artifact.
    artifactType :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the artifact.
    artifactArn :: Core.Maybe Core.Text,
    -- | The source of the artifact.
    source :: Core.Maybe ArtifactSource,
    -- | A list of the artifact\'s properties.
    properties :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | When the artifact was last modified.
    lastModifiedTime :: Core.Maybe Core.POSIX,
    createdBy :: Core.Maybe UserContext,
    lastModifiedBy :: Core.Maybe UserContext,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeArtifactResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metadataProperties', 'describeArtifactResponse_metadataProperties' - Undocumented member.
--
-- 'creationTime', 'describeArtifactResponse_creationTime' - When the artifact was created.
--
-- 'artifactName', 'describeArtifactResponse_artifactName' - The name of the artifact.
--
-- 'artifactType', 'describeArtifactResponse_artifactType' - The type of the artifact.
--
-- 'artifactArn', 'describeArtifactResponse_artifactArn' - The Amazon Resource Name (ARN) of the artifact.
--
-- 'source', 'describeArtifactResponse_source' - The source of the artifact.
--
-- 'properties', 'describeArtifactResponse_properties' - A list of the artifact\'s properties.
--
-- 'lastModifiedTime', 'describeArtifactResponse_lastModifiedTime' - When the artifact was last modified.
--
-- 'createdBy', 'describeArtifactResponse_createdBy' - Undocumented member.
--
-- 'lastModifiedBy', 'describeArtifactResponse_lastModifiedBy' - Undocumented member.
--
-- 'httpStatus', 'describeArtifactResponse_httpStatus' - The response's http status code.
newDescribeArtifactResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeArtifactResponse
newDescribeArtifactResponse pHttpStatus_ =
  DescribeArtifactResponse'
    { metadataProperties =
        Core.Nothing,
      creationTime = Core.Nothing,
      artifactName = Core.Nothing,
      artifactType = Core.Nothing,
      artifactArn = Core.Nothing,
      source = Core.Nothing,
      properties = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      createdBy = Core.Nothing,
      lastModifiedBy = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
describeArtifactResponse_metadataProperties :: Lens.Lens' DescribeArtifactResponse (Core.Maybe MetadataProperties)
describeArtifactResponse_metadataProperties = Lens.lens (\DescribeArtifactResponse' {metadataProperties} -> metadataProperties) (\s@DescribeArtifactResponse' {} a -> s {metadataProperties = a} :: DescribeArtifactResponse)

-- | When the artifact was created.
describeArtifactResponse_creationTime :: Lens.Lens' DescribeArtifactResponse (Core.Maybe Core.UTCTime)
describeArtifactResponse_creationTime = Lens.lens (\DescribeArtifactResponse' {creationTime} -> creationTime) (\s@DescribeArtifactResponse' {} a -> s {creationTime = a} :: DescribeArtifactResponse) Core.. Lens.mapping Core._Time

-- | The name of the artifact.
describeArtifactResponse_artifactName :: Lens.Lens' DescribeArtifactResponse (Core.Maybe Core.Text)
describeArtifactResponse_artifactName = Lens.lens (\DescribeArtifactResponse' {artifactName} -> artifactName) (\s@DescribeArtifactResponse' {} a -> s {artifactName = a} :: DescribeArtifactResponse)

-- | The type of the artifact.
describeArtifactResponse_artifactType :: Lens.Lens' DescribeArtifactResponse (Core.Maybe Core.Text)
describeArtifactResponse_artifactType = Lens.lens (\DescribeArtifactResponse' {artifactType} -> artifactType) (\s@DescribeArtifactResponse' {} a -> s {artifactType = a} :: DescribeArtifactResponse)

-- | The Amazon Resource Name (ARN) of the artifact.
describeArtifactResponse_artifactArn :: Lens.Lens' DescribeArtifactResponse (Core.Maybe Core.Text)
describeArtifactResponse_artifactArn = Lens.lens (\DescribeArtifactResponse' {artifactArn} -> artifactArn) (\s@DescribeArtifactResponse' {} a -> s {artifactArn = a} :: DescribeArtifactResponse)

-- | The source of the artifact.
describeArtifactResponse_source :: Lens.Lens' DescribeArtifactResponse (Core.Maybe ArtifactSource)
describeArtifactResponse_source = Lens.lens (\DescribeArtifactResponse' {source} -> source) (\s@DescribeArtifactResponse' {} a -> s {source = a} :: DescribeArtifactResponse)

-- | A list of the artifact\'s properties.
describeArtifactResponse_properties :: Lens.Lens' DescribeArtifactResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
describeArtifactResponse_properties = Lens.lens (\DescribeArtifactResponse' {properties} -> properties) (\s@DescribeArtifactResponse' {} a -> s {properties = a} :: DescribeArtifactResponse) Core.. Lens.mapping Lens._Coerce

-- | When the artifact was last modified.
describeArtifactResponse_lastModifiedTime :: Lens.Lens' DescribeArtifactResponse (Core.Maybe Core.UTCTime)
describeArtifactResponse_lastModifiedTime = Lens.lens (\DescribeArtifactResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeArtifactResponse' {} a -> s {lastModifiedTime = a} :: DescribeArtifactResponse) Core.. Lens.mapping Core._Time

-- | Undocumented member.
describeArtifactResponse_createdBy :: Lens.Lens' DescribeArtifactResponse (Core.Maybe UserContext)
describeArtifactResponse_createdBy = Lens.lens (\DescribeArtifactResponse' {createdBy} -> createdBy) (\s@DescribeArtifactResponse' {} a -> s {createdBy = a} :: DescribeArtifactResponse)

-- | Undocumented member.
describeArtifactResponse_lastModifiedBy :: Lens.Lens' DescribeArtifactResponse (Core.Maybe UserContext)
describeArtifactResponse_lastModifiedBy = Lens.lens (\DescribeArtifactResponse' {lastModifiedBy} -> lastModifiedBy) (\s@DescribeArtifactResponse' {} a -> s {lastModifiedBy = a} :: DescribeArtifactResponse)

-- | The response's http status code.
describeArtifactResponse_httpStatus :: Lens.Lens' DescribeArtifactResponse Core.Int
describeArtifactResponse_httpStatus = Lens.lens (\DescribeArtifactResponse' {httpStatus} -> httpStatus) (\s@DescribeArtifactResponse' {} a -> s {httpStatus = a} :: DescribeArtifactResponse)

instance Core.NFData DescribeArtifactResponse
