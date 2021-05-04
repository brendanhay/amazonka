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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDescribeArtifact' smart constructor.
data DescribeArtifact = DescribeArtifact'
  { -- | The Amazon Resource Name (ARN) of the artifact to describe.
    artifactArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DescribeArtifact where
  type Rs DescribeArtifact = DescribeArtifactResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeArtifactResponse'
            Prelude.<$> (x Prelude..?> "MetadataProperties")
            Prelude.<*> (x Prelude..?> "CreationTime")
            Prelude.<*> (x Prelude..?> "ArtifactName")
            Prelude.<*> (x Prelude..?> "ArtifactType")
            Prelude.<*> (x Prelude..?> "ArtifactArn")
            Prelude.<*> (x Prelude..?> "Source")
            Prelude.<*> ( x Prelude..?> "Properties"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (x Prelude..?> "LastModifiedTime")
            Prelude.<*> (x Prelude..?> "CreatedBy")
            Prelude.<*> (x Prelude..?> "LastModifiedBy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeArtifact

instance Prelude.NFData DescribeArtifact

instance Prelude.ToHeaders DescribeArtifact where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("SageMaker.DescribeArtifact" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeArtifact where
  toJSON DescribeArtifact' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ArtifactArn" Prelude..= artifactArn)
          ]
      )

instance Prelude.ToPath DescribeArtifact where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeArtifact where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeArtifactResponse' smart constructor.
data DescribeArtifactResponse = DescribeArtifactResponse'
  { metadataProperties :: Prelude.Maybe MetadataProperties,
    -- | When the artifact was created.
    creationTime :: Prelude.Maybe Prelude.POSIX,
    -- | The name of the artifact.
    artifactName :: Prelude.Maybe Prelude.Text,
    -- | The type of the artifact.
    artifactType :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the artifact.
    artifactArn :: Prelude.Maybe Prelude.Text,
    -- | The source of the artifact.
    source :: Prelude.Maybe ArtifactSource,
    -- | A list of the artifact\'s properties.
    properties :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | When the artifact was last modified.
    lastModifiedTime :: Prelude.Maybe Prelude.POSIX,
    createdBy :: Prelude.Maybe UserContext,
    lastModifiedBy :: Prelude.Maybe UserContext,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DescribeArtifactResponse
newDescribeArtifactResponse pHttpStatus_ =
  DescribeArtifactResponse'
    { metadataProperties =
        Prelude.Nothing,
      creationTime = Prelude.Nothing,
      artifactName = Prelude.Nothing,
      artifactType = Prelude.Nothing,
      artifactArn = Prelude.Nothing,
      source = Prelude.Nothing,
      properties = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
describeArtifactResponse_metadataProperties :: Lens.Lens' DescribeArtifactResponse (Prelude.Maybe MetadataProperties)
describeArtifactResponse_metadataProperties = Lens.lens (\DescribeArtifactResponse' {metadataProperties} -> metadataProperties) (\s@DescribeArtifactResponse' {} a -> s {metadataProperties = a} :: DescribeArtifactResponse)

-- | When the artifact was created.
describeArtifactResponse_creationTime :: Lens.Lens' DescribeArtifactResponse (Prelude.Maybe Prelude.UTCTime)
describeArtifactResponse_creationTime = Lens.lens (\DescribeArtifactResponse' {creationTime} -> creationTime) (\s@DescribeArtifactResponse' {} a -> s {creationTime = a} :: DescribeArtifactResponse) Prelude.. Lens.mapping Prelude._Time

-- | The name of the artifact.
describeArtifactResponse_artifactName :: Lens.Lens' DescribeArtifactResponse (Prelude.Maybe Prelude.Text)
describeArtifactResponse_artifactName = Lens.lens (\DescribeArtifactResponse' {artifactName} -> artifactName) (\s@DescribeArtifactResponse' {} a -> s {artifactName = a} :: DescribeArtifactResponse)

-- | The type of the artifact.
describeArtifactResponse_artifactType :: Lens.Lens' DescribeArtifactResponse (Prelude.Maybe Prelude.Text)
describeArtifactResponse_artifactType = Lens.lens (\DescribeArtifactResponse' {artifactType} -> artifactType) (\s@DescribeArtifactResponse' {} a -> s {artifactType = a} :: DescribeArtifactResponse)

-- | The Amazon Resource Name (ARN) of the artifact.
describeArtifactResponse_artifactArn :: Lens.Lens' DescribeArtifactResponse (Prelude.Maybe Prelude.Text)
describeArtifactResponse_artifactArn = Lens.lens (\DescribeArtifactResponse' {artifactArn} -> artifactArn) (\s@DescribeArtifactResponse' {} a -> s {artifactArn = a} :: DescribeArtifactResponse)

-- | The source of the artifact.
describeArtifactResponse_source :: Lens.Lens' DescribeArtifactResponse (Prelude.Maybe ArtifactSource)
describeArtifactResponse_source = Lens.lens (\DescribeArtifactResponse' {source} -> source) (\s@DescribeArtifactResponse' {} a -> s {source = a} :: DescribeArtifactResponse)

-- | A list of the artifact\'s properties.
describeArtifactResponse_properties :: Lens.Lens' DescribeArtifactResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeArtifactResponse_properties = Lens.lens (\DescribeArtifactResponse' {properties} -> properties) (\s@DescribeArtifactResponse' {} a -> s {properties = a} :: DescribeArtifactResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | When the artifact was last modified.
describeArtifactResponse_lastModifiedTime :: Lens.Lens' DescribeArtifactResponse (Prelude.Maybe Prelude.UTCTime)
describeArtifactResponse_lastModifiedTime = Lens.lens (\DescribeArtifactResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeArtifactResponse' {} a -> s {lastModifiedTime = a} :: DescribeArtifactResponse) Prelude.. Lens.mapping Prelude._Time

-- | Undocumented member.
describeArtifactResponse_createdBy :: Lens.Lens' DescribeArtifactResponse (Prelude.Maybe UserContext)
describeArtifactResponse_createdBy = Lens.lens (\DescribeArtifactResponse' {createdBy} -> createdBy) (\s@DescribeArtifactResponse' {} a -> s {createdBy = a} :: DescribeArtifactResponse)

-- | Undocumented member.
describeArtifactResponse_lastModifiedBy :: Lens.Lens' DescribeArtifactResponse (Prelude.Maybe UserContext)
describeArtifactResponse_lastModifiedBy = Lens.lens (\DescribeArtifactResponse' {lastModifiedBy} -> lastModifiedBy) (\s@DescribeArtifactResponse' {} a -> s {lastModifiedBy = a} :: DescribeArtifactResponse)

-- | The response's http status code.
describeArtifactResponse_httpStatus :: Lens.Lens' DescribeArtifactResponse Prelude.Int
describeArtifactResponse_httpStatus = Lens.lens (\DescribeArtifactResponse' {httpStatus} -> httpStatus) (\s@DescribeArtifactResponse' {} a -> s {httpStatus = a} :: DescribeArtifactResponse)

instance Prelude.NFData DescribeArtifactResponse
