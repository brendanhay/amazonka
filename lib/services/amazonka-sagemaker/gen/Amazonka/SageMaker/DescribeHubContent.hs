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
-- Module      : Amazonka.SageMaker.DescribeHubContent
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe the content of a hub.
module Amazonka.SageMaker.DescribeHubContent
  ( -- * Creating a Request
    DescribeHubContent (..),
    newDescribeHubContent,

    -- * Request Lenses
    describeHubContent_hubContentVersion,
    describeHubContent_hubName,
    describeHubContent_hubContentType,
    describeHubContent_hubContentName,

    -- * Destructuring the Response
    DescribeHubContentResponse (..),
    newDescribeHubContentResponse,

    -- * Response Lenses
    describeHubContentResponse_failureReason,
    describeHubContentResponse_hubContentDependencies,
    describeHubContentResponse_hubContentDescription,
    describeHubContentResponse_hubContentDisplayName,
    describeHubContentResponse_hubContentMarkdown,
    describeHubContentResponse_hubContentSearchKeywords,
    describeHubContentResponse_httpStatus,
    describeHubContentResponse_hubContentName,
    describeHubContentResponse_hubContentArn,
    describeHubContentResponse_hubContentVersion,
    describeHubContentResponse_hubContentType,
    describeHubContentResponse_documentSchemaVersion,
    describeHubContentResponse_hubName,
    describeHubContentResponse_hubArn,
    describeHubContentResponse_hubContentDocument,
    describeHubContentResponse_hubContentStatus,
    describeHubContentResponse_creationTime,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDescribeHubContent' smart constructor.
data DescribeHubContent = DescribeHubContent'
  { -- | The version of the content to describe.
    hubContentVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the hub that contains the content to describe.
    hubName :: Prelude.Text,
    -- | The type of content in the hub.
    hubContentType :: HubContentType,
    -- | The name of the content to describe.
    hubContentName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeHubContent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hubContentVersion', 'describeHubContent_hubContentVersion' - The version of the content to describe.
--
-- 'hubName', 'describeHubContent_hubName' - The name of the hub that contains the content to describe.
--
-- 'hubContentType', 'describeHubContent_hubContentType' - The type of content in the hub.
--
-- 'hubContentName', 'describeHubContent_hubContentName' - The name of the content to describe.
newDescribeHubContent ::
  -- | 'hubName'
  Prelude.Text ->
  -- | 'hubContentType'
  HubContentType ->
  -- | 'hubContentName'
  Prelude.Text ->
  DescribeHubContent
newDescribeHubContent
  pHubName_
  pHubContentType_
  pHubContentName_ =
    DescribeHubContent'
      { hubContentVersion =
          Prelude.Nothing,
        hubName = pHubName_,
        hubContentType = pHubContentType_,
        hubContentName = pHubContentName_
      }

-- | The version of the content to describe.
describeHubContent_hubContentVersion :: Lens.Lens' DescribeHubContent (Prelude.Maybe Prelude.Text)
describeHubContent_hubContentVersion = Lens.lens (\DescribeHubContent' {hubContentVersion} -> hubContentVersion) (\s@DescribeHubContent' {} a -> s {hubContentVersion = a} :: DescribeHubContent)

-- | The name of the hub that contains the content to describe.
describeHubContent_hubName :: Lens.Lens' DescribeHubContent Prelude.Text
describeHubContent_hubName = Lens.lens (\DescribeHubContent' {hubName} -> hubName) (\s@DescribeHubContent' {} a -> s {hubName = a} :: DescribeHubContent)

-- | The type of content in the hub.
describeHubContent_hubContentType :: Lens.Lens' DescribeHubContent HubContentType
describeHubContent_hubContentType = Lens.lens (\DescribeHubContent' {hubContentType} -> hubContentType) (\s@DescribeHubContent' {} a -> s {hubContentType = a} :: DescribeHubContent)

-- | The name of the content to describe.
describeHubContent_hubContentName :: Lens.Lens' DescribeHubContent Prelude.Text
describeHubContent_hubContentName = Lens.lens (\DescribeHubContent' {hubContentName} -> hubContentName) (\s@DescribeHubContent' {} a -> s {hubContentName = a} :: DescribeHubContent)

instance Core.AWSRequest DescribeHubContent where
  type
    AWSResponse DescribeHubContent =
      DescribeHubContentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeHubContentResponse'
            Prelude.<$> (x Data..?> "FailureReason")
            Prelude.<*> ( x Data..?> "HubContentDependencies"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "HubContentDescription")
            Prelude.<*> (x Data..?> "HubContentDisplayName")
            Prelude.<*> (x Data..?> "HubContentMarkdown")
            Prelude.<*> ( x Data..?> "HubContentSearchKeywords"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "HubContentName")
            Prelude.<*> (x Data..:> "HubContentArn")
            Prelude.<*> (x Data..:> "HubContentVersion")
            Prelude.<*> (x Data..:> "HubContentType")
            Prelude.<*> (x Data..:> "DocumentSchemaVersion")
            Prelude.<*> (x Data..:> "HubName")
            Prelude.<*> (x Data..:> "HubArn")
            Prelude.<*> (x Data..:> "HubContentDocument")
            Prelude.<*> (x Data..:> "HubContentStatus")
            Prelude.<*> (x Data..:> "CreationTime")
      )

instance Prelude.Hashable DescribeHubContent where
  hashWithSalt _salt DescribeHubContent' {..} =
    _salt `Prelude.hashWithSalt` hubContentVersion
      `Prelude.hashWithSalt` hubName
      `Prelude.hashWithSalt` hubContentType
      `Prelude.hashWithSalt` hubContentName

instance Prelude.NFData DescribeHubContent where
  rnf DescribeHubContent' {..} =
    Prelude.rnf hubContentVersion
      `Prelude.seq` Prelude.rnf hubName
      `Prelude.seq` Prelude.rnf hubContentType
      `Prelude.seq` Prelude.rnf hubContentName

instance Data.ToHeaders DescribeHubContent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.DescribeHubContent" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeHubContent where
  toJSON DescribeHubContent' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("HubContentVersion" Data..=)
              Prelude.<$> hubContentVersion,
            Prelude.Just ("HubName" Data..= hubName),
            Prelude.Just
              ("HubContentType" Data..= hubContentType),
            Prelude.Just
              ("HubContentName" Data..= hubContentName)
          ]
      )

instance Data.ToPath DescribeHubContent where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeHubContent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeHubContentResponse' smart constructor.
data DescribeHubContentResponse = DescribeHubContentResponse'
  { -- | The failure reason if importing hub content failed.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The location of any dependencies that the hub content has, such as
    -- scripts, model artifacts, datasets, or notebooks.
    hubContentDependencies :: Prelude.Maybe [HubContentDependency],
    -- | A description of the hub content.
    hubContentDescription :: Prelude.Maybe Prelude.Text,
    -- | The display name of the hub content.
    hubContentDisplayName :: Prelude.Maybe Prelude.Text,
    -- | Markdown files associated with the hub content to import.
    hubContentMarkdown :: Prelude.Maybe Prelude.Text,
    -- | The searchable keywords for the hub content.
    hubContentSearchKeywords :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the hub content.
    hubContentName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the hub content.
    hubContentArn :: Prelude.Text,
    -- | The version of the hub content.
    hubContentVersion :: Prelude.Text,
    -- | The type of hub content.
    hubContentType :: HubContentType,
    -- | The document schema version for the hub content.
    documentSchemaVersion :: Prelude.Text,
    -- | The name of the hub that contains the content.
    hubName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the hub that contains the content.
    hubArn :: Prelude.Text,
    -- | The hub content document that describes information about the hub
    -- content such as type, associated containers, scripts, and more.
    hubContentDocument :: Prelude.Text,
    -- | The status of the hub content.
    hubContentStatus :: HubContentStatus,
    -- | The date and time that hub content was created.
    creationTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeHubContentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureReason', 'describeHubContentResponse_failureReason' - The failure reason if importing hub content failed.
--
-- 'hubContentDependencies', 'describeHubContentResponse_hubContentDependencies' - The location of any dependencies that the hub content has, such as
-- scripts, model artifacts, datasets, or notebooks.
--
-- 'hubContentDescription', 'describeHubContentResponse_hubContentDescription' - A description of the hub content.
--
-- 'hubContentDisplayName', 'describeHubContentResponse_hubContentDisplayName' - The display name of the hub content.
--
-- 'hubContentMarkdown', 'describeHubContentResponse_hubContentMarkdown' - Markdown files associated with the hub content to import.
--
-- 'hubContentSearchKeywords', 'describeHubContentResponse_hubContentSearchKeywords' - The searchable keywords for the hub content.
--
-- 'httpStatus', 'describeHubContentResponse_httpStatus' - The response's http status code.
--
-- 'hubContentName', 'describeHubContentResponse_hubContentName' - The name of the hub content.
--
-- 'hubContentArn', 'describeHubContentResponse_hubContentArn' - The Amazon Resource Name (ARN) of the hub content.
--
-- 'hubContentVersion', 'describeHubContentResponse_hubContentVersion' - The version of the hub content.
--
-- 'hubContentType', 'describeHubContentResponse_hubContentType' - The type of hub content.
--
-- 'documentSchemaVersion', 'describeHubContentResponse_documentSchemaVersion' - The document schema version for the hub content.
--
-- 'hubName', 'describeHubContentResponse_hubName' - The name of the hub that contains the content.
--
-- 'hubArn', 'describeHubContentResponse_hubArn' - The Amazon Resource Name (ARN) of the hub that contains the content.
--
-- 'hubContentDocument', 'describeHubContentResponse_hubContentDocument' - The hub content document that describes information about the hub
-- content such as type, associated containers, scripts, and more.
--
-- 'hubContentStatus', 'describeHubContentResponse_hubContentStatus' - The status of the hub content.
--
-- 'creationTime', 'describeHubContentResponse_creationTime' - The date and time that hub content was created.
newDescribeHubContentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'hubContentName'
  Prelude.Text ->
  -- | 'hubContentArn'
  Prelude.Text ->
  -- | 'hubContentVersion'
  Prelude.Text ->
  -- | 'hubContentType'
  HubContentType ->
  -- | 'documentSchemaVersion'
  Prelude.Text ->
  -- | 'hubName'
  Prelude.Text ->
  -- | 'hubArn'
  Prelude.Text ->
  -- | 'hubContentDocument'
  Prelude.Text ->
  -- | 'hubContentStatus'
  HubContentStatus ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  DescribeHubContentResponse
newDescribeHubContentResponse
  pHttpStatus_
  pHubContentName_
  pHubContentArn_
  pHubContentVersion_
  pHubContentType_
  pDocumentSchemaVersion_
  pHubName_
  pHubArn_
  pHubContentDocument_
  pHubContentStatus_
  pCreationTime_ =
    DescribeHubContentResponse'
      { failureReason =
          Prelude.Nothing,
        hubContentDependencies = Prelude.Nothing,
        hubContentDescription = Prelude.Nothing,
        hubContentDisplayName = Prelude.Nothing,
        hubContentMarkdown = Prelude.Nothing,
        hubContentSearchKeywords = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        hubContentName = pHubContentName_,
        hubContentArn = pHubContentArn_,
        hubContentVersion = pHubContentVersion_,
        hubContentType = pHubContentType_,
        documentSchemaVersion = pDocumentSchemaVersion_,
        hubName = pHubName_,
        hubArn = pHubArn_,
        hubContentDocument = pHubContentDocument_,
        hubContentStatus = pHubContentStatus_,
        creationTime = Data._Time Lens.# pCreationTime_
      }

-- | The failure reason if importing hub content failed.
describeHubContentResponse_failureReason :: Lens.Lens' DescribeHubContentResponse (Prelude.Maybe Prelude.Text)
describeHubContentResponse_failureReason = Lens.lens (\DescribeHubContentResponse' {failureReason} -> failureReason) (\s@DescribeHubContentResponse' {} a -> s {failureReason = a} :: DescribeHubContentResponse)

-- | The location of any dependencies that the hub content has, such as
-- scripts, model artifacts, datasets, or notebooks.
describeHubContentResponse_hubContentDependencies :: Lens.Lens' DescribeHubContentResponse (Prelude.Maybe [HubContentDependency])
describeHubContentResponse_hubContentDependencies = Lens.lens (\DescribeHubContentResponse' {hubContentDependencies} -> hubContentDependencies) (\s@DescribeHubContentResponse' {} a -> s {hubContentDependencies = a} :: DescribeHubContentResponse) Prelude.. Lens.mapping Lens.coerced

-- | A description of the hub content.
describeHubContentResponse_hubContentDescription :: Lens.Lens' DescribeHubContentResponse (Prelude.Maybe Prelude.Text)
describeHubContentResponse_hubContentDescription = Lens.lens (\DescribeHubContentResponse' {hubContentDescription} -> hubContentDescription) (\s@DescribeHubContentResponse' {} a -> s {hubContentDescription = a} :: DescribeHubContentResponse)

-- | The display name of the hub content.
describeHubContentResponse_hubContentDisplayName :: Lens.Lens' DescribeHubContentResponse (Prelude.Maybe Prelude.Text)
describeHubContentResponse_hubContentDisplayName = Lens.lens (\DescribeHubContentResponse' {hubContentDisplayName} -> hubContentDisplayName) (\s@DescribeHubContentResponse' {} a -> s {hubContentDisplayName = a} :: DescribeHubContentResponse)

-- | Markdown files associated with the hub content to import.
describeHubContentResponse_hubContentMarkdown :: Lens.Lens' DescribeHubContentResponse (Prelude.Maybe Prelude.Text)
describeHubContentResponse_hubContentMarkdown = Lens.lens (\DescribeHubContentResponse' {hubContentMarkdown} -> hubContentMarkdown) (\s@DescribeHubContentResponse' {} a -> s {hubContentMarkdown = a} :: DescribeHubContentResponse)

-- | The searchable keywords for the hub content.
describeHubContentResponse_hubContentSearchKeywords :: Lens.Lens' DescribeHubContentResponse (Prelude.Maybe [Prelude.Text])
describeHubContentResponse_hubContentSearchKeywords = Lens.lens (\DescribeHubContentResponse' {hubContentSearchKeywords} -> hubContentSearchKeywords) (\s@DescribeHubContentResponse' {} a -> s {hubContentSearchKeywords = a} :: DescribeHubContentResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeHubContentResponse_httpStatus :: Lens.Lens' DescribeHubContentResponse Prelude.Int
describeHubContentResponse_httpStatus = Lens.lens (\DescribeHubContentResponse' {httpStatus} -> httpStatus) (\s@DescribeHubContentResponse' {} a -> s {httpStatus = a} :: DescribeHubContentResponse)

-- | The name of the hub content.
describeHubContentResponse_hubContentName :: Lens.Lens' DescribeHubContentResponse Prelude.Text
describeHubContentResponse_hubContentName = Lens.lens (\DescribeHubContentResponse' {hubContentName} -> hubContentName) (\s@DescribeHubContentResponse' {} a -> s {hubContentName = a} :: DescribeHubContentResponse)

-- | The Amazon Resource Name (ARN) of the hub content.
describeHubContentResponse_hubContentArn :: Lens.Lens' DescribeHubContentResponse Prelude.Text
describeHubContentResponse_hubContentArn = Lens.lens (\DescribeHubContentResponse' {hubContentArn} -> hubContentArn) (\s@DescribeHubContentResponse' {} a -> s {hubContentArn = a} :: DescribeHubContentResponse)

-- | The version of the hub content.
describeHubContentResponse_hubContentVersion :: Lens.Lens' DescribeHubContentResponse Prelude.Text
describeHubContentResponse_hubContentVersion = Lens.lens (\DescribeHubContentResponse' {hubContentVersion} -> hubContentVersion) (\s@DescribeHubContentResponse' {} a -> s {hubContentVersion = a} :: DescribeHubContentResponse)

-- | The type of hub content.
describeHubContentResponse_hubContentType :: Lens.Lens' DescribeHubContentResponse HubContentType
describeHubContentResponse_hubContentType = Lens.lens (\DescribeHubContentResponse' {hubContentType} -> hubContentType) (\s@DescribeHubContentResponse' {} a -> s {hubContentType = a} :: DescribeHubContentResponse)

-- | The document schema version for the hub content.
describeHubContentResponse_documentSchemaVersion :: Lens.Lens' DescribeHubContentResponse Prelude.Text
describeHubContentResponse_documentSchemaVersion = Lens.lens (\DescribeHubContentResponse' {documentSchemaVersion} -> documentSchemaVersion) (\s@DescribeHubContentResponse' {} a -> s {documentSchemaVersion = a} :: DescribeHubContentResponse)

-- | The name of the hub that contains the content.
describeHubContentResponse_hubName :: Lens.Lens' DescribeHubContentResponse Prelude.Text
describeHubContentResponse_hubName = Lens.lens (\DescribeHubContentResponse' {hubName} -> hubName) (\s@DescribeHubContentResponse' {} a -> s {hubName = a} :: DescribeHubContentResponse)

-- | The Amazon Resource Name (ARN) of the hub that contains the content.
describeHubContentResponse_hubArn :: Lens.Lens' DescribeHubContentResponse Prelude.Text
describeHubContentResponse_hubArn = Lens.lens (\DescribeHubContentResponse' {hubArn} -> hubArn) (\s@DescribeHubContentResponse' {} a -> s {hubArn = a} :: DescribeHubContentResponse)

-- | The hub content document that describes information about the hub
-- content such as type, associated containers, scripts, and more.
describeHubContentResponse_hubContentDocument :: Lens.Lens' DescribeHubContentResponse Prelude.Text
describeHubContentResponse_hubContentDocument = Lens.lens (\DescribeHubContentResponse' {hubContentDocument} -> hubContentDocument) (\s@DescribeHubContentResponse' {} a -> s {hubContentDocument = a} :: DescribeHubContentResponse)

-- | The status of the hub content.
describeHubContentResponse_hubContentStatus :: Lens.Lens' DescribeHubContentResponse HubContentStatus
describeHubContentResponse_hubContentStatus = Lens.lens (\DescribeHubContentResponse' {hubContentStatus} -> hubContentStatus) (\s@DescribeHubContentResponse' {} a -> s {hubContentStatus = a} :: DescribeHubContentResponse)

-- | The date and time that hub content was created.
describeHubContentResponse_creationTime :: Lens.Lens' DescribeHubContentResponse Prelude.UTCTime
describeHubContentResponse_creationTime = Lens.lens (\DescribeHubContentResponse' {creationTime} -> creationTime) (\s@DescribeHubContentResponse' {} a -> s {creationTime = a} :: DescribeHubContentResponse) Prelude.. Data._Time

instance Prelude.NFData DescribeHubContentResponse where
  rnf DescribeHubContentResponse' {..} =
    Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf hubContentDependencies
      `Prelude.seq` Prelude.rnf hubContentDescription
      `Prelude.seq` Prelude.rnf hubContentDisplayName
      `Prelude.seq` Prelude.rnf hubContentMarkdown
      `Prelude.seq` Prelude.rnf hubContentSearchKeywords
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf hubContentName
      `Prelude.seq` Prelude.rnf hubContentArn
      `Prelude.seq` Prelude.rnf hubContentVersion
      `Prelude.seq` Prelude.rnf hubContentType
      `Prelude.seq` Prelude.rnf documentSchemaVersion
      `Prelude.seq` Prelude.rnf hubName
      `Prelude.seq` Prelude.rnf hubArn
      `Prelude.seq` Prelude.rnf hubContentDocument
      `Prelude.seq` Prelude.rnf hubContentStatus
      `Prelude.seq` Prelude.rnf creationTime
