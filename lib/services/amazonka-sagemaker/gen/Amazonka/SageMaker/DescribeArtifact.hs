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
-- Module      : Amazonka.SageMaker.DescribeArtifact
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an artifact.
module Amazonka.SageMaker.DescribeArtifact
  ( -- * Creating a Request
    DescribeArtifact (..),
    newDescribeArtifact,

    -- * Request Lenses
    describeArtifact_artifactArn,

    -- * Destructuring the Response
    DescribeArtifactResponse (..),
    newDescribeArtifactResponse,

    -- * Response Lenses
    describeArtifactResponse_creationTime,
    describeArtifactResponse_metadataProperties,
    describeArtifactResponse_createdBy,
    describeArtifactResponse_lastModifiedTime,
    describeArtifactResponse_artifactName,
    describeArtifactResponse_source,
    describeArtifactResponse_lastModifiedBy,
    describeArtifactResponse_artifactArn,
    describeArtifactResponse_artifactType,
    describeArtifactResponse_properties,
    describeArtifactResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDescribeArtifact' smart constructor.
data DescribeArtifact = DescribeArtifact'
  { -- | The Amazon Resource Name (ARN) of the artifact to describe.
    artifactArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DescribeArtifact
newDescribeArtifact pArtifactArn_ =
  DescribeArtifact' {artifactArn = pArtifactArn_}

-- | The Amazon Resource Name (ARN) of the artifact to describe.
describeArtifact_artifactArn :: Lens.Lens' DescribeArtifact Prelude.Text
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
            Prelude.<$> (x Core..?> "CreationTime")
            Prelude.<*> (x Core..?> "MetadataProperties")
            Prelude.<*> (x Core..?> "CreatedBy")
            Prelude.<*> (x Core..?> "LastModifiedTime")
            Prelude.<*> (x Core..?> "ArtifactName")
            Prelude.<*> (x Core..?> "Source")
            Prelude.<*> (x Core..?> "LastModifiedBy")
            Prelude.<*> (x Core..?> "ArtifactArn")
            Prelude.<*> (x Core..?> "ArtifactType")
            Prelude.<*> (x Core..?> "Properties" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeArtifact where
  hashWithSalt salt' DescribeArtifact' {..} =
    salt' `Prelude.hashWithSalt` artifactArn

instance Prelude.NFData DescribeArtifact where
  rnf DescribeArtifact' {..} = Prelude.rnf artifactArn

instance Core.ToHeaders DescribeArtifact where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.DescribeArtifact" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeArtifact where
  toJSON DescribeArtifact' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("ArtifactArn" Core..= artifactArn)]
      )

instance Core.ToPath DescribeArtifact where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeArtifact where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeArtifactResponse' smart constructor.
data DescribeArtifactResponse = DescribeArtifactResponse'
  { -- | When the artifact was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    metadataProperties :: Prelude.Maybe MetadataProperties,
    createdBy :: Prelude.Maybe UserContext,
    -- | When the artifact was last modified.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the artifact.
    artifactName :: Prelude.Maybe Prelude.Text,
    -- | The source of the artifact.
    source :: Prelude.Maybe ArtifactSource,
    lastModifiedBy :: Prelude.Maybe UserContext,
    -- | The Amazon Resource Name (ARN) of the artifact.
    artifactArn :: Prelude.Maybe Prelude.Text,
    -- | The type of the artifact.
    artifactType :: Prelude.Maybe Prelude.Text,
    -- | A list of the artifact\'s properties.
    properties :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeArtifactResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'describeArtifactResponse_creationTime' - When the artifact was created.
--
-- 'metadataProperties', 'describeArtifactResponse_metadataProperties' - Undocumented member.
--
-- 'createdBy', 'describeArtifactResponse_createdBy' - Undocumented member.
--
-- 'lastModifiedTime', 'describeArtifactResponse_lastModifiedTime' - When the artifact was last modified.
--
-- 'artifactName', 'describeArtifactResponse_artifactName' - The name of the artifact.
--
-- 'source', 'describeArtifactResponse_source' - The source of the artifact.
--
-- 'lastModifiedBy', 'describeArtifactResponse_lastModifiedBy' - Undocumented member.
--
-- 'artifactArn', 'describeArtifactResponse_artifactArn' - The Amazon Resource Name (ARN) of the artifact.
--
-- 'artifactType', 'describeArtifactResponse_artifactType' - The type of the artifact.
--
-- 'properties', 'describeArtifactResponse_properties' - A list of the artifact\'s properties.
--
-- 'httpStatus', 'describeArtifactResponse_httpStatus' - The response's http status code.
newDescribeArtifactResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeArtifactResponse
newDescribeArtifactResponse pHttpStatus_ =
  DescribeArtifactResponse'
    { creationTime =
        Prelude.Nothing,
      metadataProperties = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      artifactName = Prelude.Nothing,
      source = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      artifactArn = Prelude.Nothing,
      artifactType = Prelude.Nothing,
      properties = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | When the artifact was created.
describeArtifactResponse_creationTime :: Lens.Lens' DescribeArtifactResponse (Prelude.Maybe Prelude.UTCTime)
describeArtifactResponse_creationTime = Lens.lens (\DescribeArtifactResponse' {creationTime} -> creationTime) (\s@DescribeArtifactResponse' {} a -> s {creationTime = a} :: DescribeArtifactResponse) Prelude.. Lens.mapping Core._Time

-- | Undocumented member.
describeArtifactResponse_metadataProperties :: Lens.Lens' DescribeArtifactResponse (Prelude.Maybe MetadataProperties)
describeArtifactResponse_metadataProperties = Lens.lens (\DescribeArtifactResponse' {metadataProperties} -> metadataProperties) (\s@DescribeArtifactResponse' {} a -> s {metadataProperties = a} :: DescribeArtifactResponse)

-- | Undocumented member.
describeArtifactResponse_createdBy :: Lens.Lens' DescribeArtifactResponse (Prelude.Maybe UserContext)
describeArtifactResponse_createdBy = Lens.lens (\DescribeArtifactResponse' {createdBy} -> createdBy) (\s@DescribeArtifactResponse' {} a -> s {createdBy = a} :: DescribeArtifactResponse)

-- | When the artifact was last modified.
describeArtifactResponse_lastModifiedTime :: Lens.Lens' DescribeArtifactResponse (Prelude.Maybe Prelude.UTCTime)
describeArtifactResponse_lastModifiedTime = Lens.lens (\DescribeArtifactResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeArtifactResponse' {} a -> s {lastModifiedTime = a} :: DescribeArtifactResponse) Prelude.. Lens.mapping Core._Time

-- | The name of the artifact.
describeArtifactResponse_artifactName :: Lens.Lens' DescribeArtifactResponse (Prelude.Maybe Prelude.Text)
describeArtifactResponse_artifactName = Lens.lens (\DescribeArtifactResponse' {artifactName} -> artifactName) (\s@DescribeArtifactResponse' {} a -> s {artifactName = a} :: DescribeArtifactResponse)

-- | The source of the artifact.
describeArtifactResponse_source :: Lens.Lens' DescribeArtifactResponse (Prelude.Maybe ArtifactSource)
describeArtifactResponse_source = Lens.lens (\DescribeArtifactResponse' {source} -> source) (\s@DescribeArtifactResponse' {} a -> s {source = a} :: DescribeArtifactResponse)

-- | Undocumented member.
describeArtifactResponse_lastModifiedBy :: Lens.Lens' DescribeArtifactResponse (Prelude.Maybe UserContext)
describeArtifactResponse_lastModifiedBy = Lens.lens (\DescribeArtifactResponse' {lastModifiedBy} -> lastModifiedBy) (\s@DescribeArtifactResponse' {} a -> s {lastModifiedBy = a} :: DescribeArtifactResponse)

-- | The Amazon Resource Name (ARN) of the artifact.
describeArtifactResponse_artifactArn :: Lens.Lens' DescribeArtifactResponse (Prelude.Maybe Prelude.Text)
describeArtifactResponse_artifactArn = Lens.lens (\DescribeArtifactResponse' {artifactArn} -> artifactArn) (\s@DescribeArtifactResponse' {} a -> s {artifactArn = a} :: DescribeArtifactResponse)

-- | The type of the artifact.
describeArtifactResponse_artifactType :: Lens.Lens' DescribeArtifactResponse (Prelude.Maybe Prelude.Text)
describeArtifactResponse_artifactType = Lens.lens (\DescribeArtifactResponse' {artifactType} -> artifactType) (\s@DescribeArtifactResponse' {} a -> s {artifactType = a} :: DescribeArtifactResponse)

-- | A list of the artifact\'s properties.
describeArtifactResponse_properties :: Lens.Lens' DescribeArtifactResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeArtifactResponse_properties = Lens.lens (\DescribeArtifactResponse' {properties} -> properties) (\s@DescribeArtifactResponse' {} a -> s {properties = a} :: DescribeArtifactResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeArtifactResponse_httpStatus :: Lens.Lens' DescribeArtifactResponse Prelude.Int
describeArtifactResponse_httpStatus = Lens.lens (\DescribeArtifactResponse' {httpStatus} -> httpStatus) (\s@DescribeArtifactResponse' {} a -> s {httpStatus = a} :: DescribeArtifactResponse)

instance Prelude.NFData DescribeArtifactResponse where
  rnf DescribeArtifactResponse' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf properties
      `Prelude.seq` Prelude.rnf artifactType
      `Prelude.seq` Prelude.rnf artifactArn
      `Prelude.seq` Prelude.rnf lastModifiedBy
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf artifactName
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf metadataProperties
