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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
    describeArtifactResponse_artifactArn,
    describeArtifactResponse_artifactName,
    describeArtifactResponse_artifactType,
    describeArtifactResponse_createdBy,
    describeArtifactResponse_creationTime,
    describeArtifactResponse_lastModifiedBy,
    describeArtifactResponse_lastModifiedTime,
    describeArtifactResponse_lineageGroupArn,
    describeArtifactResponse_metadataProperties,
    describeArtifactResponse_properties,
    describeArtifactResponse_source,
    describeArtifactResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeArtifactResponse'
            Prelude.<$> (x Data..?> "ArtifactArn")
            Prelude.<*> (x Data..?> "ArtifactName")
            Prelude.<*> (x Data..?> "ArtifactType")
            Prelude.<*> (x Data..?> "CreatedBy")
            Prelude.<*> (x Data..?> "CreationTime")
            Prelude.<*> (x Data..?> "LastModifiedBy")
            Prelude.<*> (x Data..?> "LastModifiedTime")
            Prelude.<*> (x Data..?> "LineageGroupArn")
            Prelude.<*> (x Data..?> "MetadataProperties")
            Prelude.<*> (x Data..?> "Properties" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "Source")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeArtifact where
  hashWithSalt _salt DescribeArtifact' {..} =
    _salt `Prelude.hashWithSalt` artifactArn

instance Prelude.NFData DescribeArtifact where
  rnf DescribeArtifact' {..} = Prelude.rnf artifactArn

instance Data.ToHeaders DescribeArtifact where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.DescribeArtifact" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeArtifact where
  toJSON DescribeArtifact' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ArtifactArn" Data..= artifactArn)]
      )

instance Data.ToPath DescribeArtifact where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeArtifact where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeArtifactResponse' smart constructor.
data DescribeArtifactResponse = DescribeArtifactResponse'
  { -- | The Amazon Resource Name (ARN) of the artifact.
    artifactArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the artifact.
    artifactName :: Prelude.Maybe Prelude.Text,
    -- | The type of the artifact.
    artifactType :: Prelude.Maybe Prelude.Text,
    createdBy :: Prelude.Maybe UserContext,
    -- | When the artifact was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    lastModifiedBy :: Prelude.Maybe UserContext,
    -- | When the artifact was last modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the lineage group.
    lineageGroupArn :: Prelude.Maybe Prelude.Text,
    metadataProperties :: Prelude.Maybe MetadataProperties,
    -- | A list of the artifact\'s properties.
    properties :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The source of the artifact.
    source :: Prelude.Maybe ArtifactSource,
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
-- 'artifactArn', 'describeArtifactResponse_artifactArn' - The Amazon Resource Name (ARN) of the artifact.
--
-- 'artifactName', 'describeArtifactResponse_artifactName' - The name of the artifact.
--
-- 'artifactType', 'describeArtifactResponse_artifactType' - The type of the artifact.
--
-- 'createdBy', 'describeArtifactResponse_createdBy' - Undocumented member.
--
-- 'creationTime', 'describeArtifactResponse_creationTime' - When the artifact was created.
--
-- 'lastModifiedBy', 'describeArtifactResponse_lastModifiedBy' - Undocumented member.
--
-- 'lastModifiedTime', 'describeArtifactResponse_lastModifiedTime' - When the artifact was last modified.
--
-- 'lineageGroupArn', 'describeArtifactResponse_lineageGroupArn' - The Amazon Resource Name (ARN) of the lineage group.
--
-- 'metadataProperties', 'describeArtifactResponse_metadataProperties' - Undocumented member.
--
-- 'properties', 'describeArtifactResponse_properties' - A list of the artifact\'s properties.
--
-- 'source', 'describeArtifactResponse_source' - The source of the artifact.
--
-- 'httpStatus', 'describeArtifactResponse_httpStatus' - The response's http status code.
newDescribeArtifactResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeArtifactResponse
newDescribeArtifactResponse pHttpStatus_ =
  DescribeArtifactResponse'
    { artifactArn =
        Prelude.Nothing,
      artifactName = Prelude.Nothing,
      artifactType = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      lineageGroupArn = Prelude.Nothing,
      metadataProperties = Prelude.Nothing,
      properties = Prelude.Nothing,
      source = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the artifact.
describeArtifactResponse_artifactArn :: Lens.Lens' DescribeArtifactResponse (Prelude.Maybe Prelude.Text)
describeArtifactResponse_artifactArn = Lens.lens (\DescribeArtifactResponse' {artifactArn} -> artifactArn) (\s@DescribeArtifactResponse' {} a -> s {artifactArn = a} :: DescribeArtifactResponse)

-- | The name of the artifact.
describeArtifactResponse_artifactName :: Lens.Lens' DescribeArtifactResponse (Prelude.Maybe Prelude.Text)
describeArtifactResponse_artifactName = Lens.lens (\DescribeArtifactResponse' {artifactName} -> artifactName) (\s@DescribeArtifactResponse' {} a -> s {artifactName = a} :: DescribeArtifactResponse)

-- | The type of the artifact.
describeArtifactResponse_artifactType :: Lens.Lens' DescribeArtifactResponse (Prelude.Maybe Prelude.Text)
describeArtifactResponse_artifactType = Lens.lens (\DescribeArtifactResponse' {artifactType} -> artifactType) (\s@DescribeArtifactResponse' {} a -> s {artifactType = a} :: DescribeArtifactResponse)

-- | Undocumented member.
describeArtifactResponse_createdBy :: Lens.Lens' DescribeArtifactResponse (Prelude.Maybe UserContext)
describeArtifactResponse_createdBy = Lens.lens (\DescribeArtifactResponse' {createdBy} -> createdBy) (\s@DescribeArtifactResponse' {} a -> s {createdBy = a} :: DescribeArtifactResponse)

-- | When the artifact was created.
describeArtifactResponse_creationTime :: Lens.Lens' DescribeArtifactResponse (Prelude.Maybe Prelude.UTCTime)
describeArtifactResponse_creationTime = Lens.lens (\DescribeArtifactResponse' {creationTime} -> creationTime) (\s@DescribeArtifactResponse' {} a -> s {creationTime = a} :: DescribeArtifactResponse) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
describeArtifactResponse_lastModifiedBy :: Lens.Lens' DescribeArtifactResponse (Prelude.Maybe UserContext)
describeArtifactResponse_lastModifiedBy = Lens.lens (\DescribeArtifactResponse' {lastModifiedBy} -> lastModifiedBy) (\s@DescribeArtifactResponse' {} a -> s {lastModifiedBy = a} :: DescribeArtifactResponse)

-- | When the artifact was last modified.
describeArtifactResponse_lastModifiedTime :: Lens.Lens' DescribeArtifactResponse (Prelude.Maybe Prelude.UTCTime)
describeArtifactResponse_lastModifiedTime = Lens.lens (\DescribeArtifactResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeArtifactResponse' {} a -> s {lastModifiedTime = a} :: DescribeArtifactResponse) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the lineage group.
describeArtifactResponse_lineageGroupArn :: Lens.Lens' DescribeArtifactResponse (Prelude.Maybe Prelude.Text)
describeArtifactResponse_lineageGroupArn = Lens.lens (\DescribeArtifactResponse' {lineageGroupArn} -> lineageGroupArn) (\s@DescribeArtifactResponse' {} a -> s {lineageGroupArn = a} :: DescribeArtifactResponse)

-- | Undocumented member.
describeArtifactResponse_metadataProperties :: Lens.Lens' DescribeArtifactResponse (Prelude.Maybe MetadataProperties)
describeArtifactResponse_metadataProperties = Lens.lens (\DescribeArtifactResponse' {metadataProperties} -> metadataProperties) (\s@DescribeArtifactResponse' {} a -> s {metadataProperties = a} :: DescribeArtifactResponse)

-- | A list of the artifact\'s properties.
describeArtifactResponse_properties :: Lens.Lens' DescribeArtifactResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeArtifactResponse_properties = Lens.lens (\DescribeArtifactResponse' {properties} -> properties) (\s@DescribeArtifactResponse' {} a -> s {properties = a} :: DescribeArtifactResponse) Prelude.. Lens.mapping Lens.coerced

-- | The source of the artifact.
describeArtifactResponse_source :: Lens.Lens' DescribeArtifactResponse (Prelude.Maybe ArtifactSource)
describeArtifactResponse_source = Lens.lens (\DescribeArtifactResponse' {source} -> source) (\s@DescribeArtifactResponse' {} a -> s {source = a} :: DescribeArtifactResponse)

-- | The response's http status code.
describeArtifactResponse_httpStatus :: Lens.Lens' DescribeArtifactResponse Prelude.Int
describeArtifactResponse_httpStatus = Lens.lens (\DescribeArtifactResponse' {httpStatus} -> httpStatus) (\s@DescribeArtifactResponse' {} a -> s {httpStatus = a} :: DescribeArtifactResponse)

instance Prelude.NFData DescribeArtifactResponse where
  rnf DescribeArtifactResponse' {..} =
    Prelude.rnf artifactArn
      `Prelude.seq` Prelude.rnf artifactName
      `Prelude.seq` Prelude.rnf artifactType
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastModifiedBy
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf lineageGroupArn
      `Prelude.seq` Prelude.rnf metadataProperties
      `Prelude.seq` Prelude.rnf properties
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf httpStatus
