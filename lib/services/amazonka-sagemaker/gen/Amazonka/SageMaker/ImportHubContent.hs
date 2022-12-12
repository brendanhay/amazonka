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
-- Module      : Amazonka.SageMaker.ImportHubContent
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Import hub content.
module Amazonka.SageMaker.ImportHubContent
  ( -- * Creating a Request
    ImportHubContent (..),
    newImportHubContent,

    -- * Request Lenses
    importHubContent_hubContentDescription,
    importHubContent_hubContentDisplayName,
    importHubContent_hubContentMarkdown,
    importHubContent_hubContentSearchKeywords,
    importHubContent_hubContentVersion,
    importHubContent_tags,
    importHubContent_hubContentName,
    importHubContent_hubContentType,
    importHubContent_documentSchemaVersion,
    importHubContent_hubName,
    importHubContent_hubContentDocument,

    -- * Destructuring the Response
    ImportHubContentResponse (..),
    newImportHubContentResponse,

    -- * Response Lenses
    importHubContentResponse_httpStatus,
    importHubContentResponse_hubArn,
    importHubContentResponse_hubContentArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newImportHubContent' smart constructor.
data ImportHubContent = ImportHubContent'
  { -- | A description of the hub content to import.
    hubContentDescription :: Prelude.Maybe Prelude.Text,
    -- | The display name of the hub content to import.
    hubContentDisplayName :: Prelude.Maybe Prelude.Text,
    -- | Markdown files associated with the hub content to import.
    hubContentMarkdown :: Prelude.Maybe Prelude.Text,
    -- | The searchable keywords of the hub content.
    hubContentSearchKeywords :: Prelude.Maybe [Prelude.Text],
    -- | The version of the hub content to import.
    hubContentVersion :: Prelude.Maybe Prelude.Text,
    -- | Any tags associated with the hub content.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the hub content to import.
    hubContentName :: Prelude.Text,
    -- | The type of hub content to import.
    hubContentType :: HubContentType,
    -- | The version of the hub content schema to import.
    documentSchemaVersion :: Prelude.Text,
    -- | The name of the hub to import content into.
    hubName :: Prelude.Text,
    -- | The hub content document that describes information about the hub
    -- content such as type, associated containers, scripts, and more.
    hubContentDocument :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportHubContent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hubContentDescription', 'importHubContent_hubContentDescription' - A description of the hub content to import.
--
-- 'hubContentDisplayName', 'importHubContent_hubContentDisplayName' - The display name of the hub content to import.
--
-- 'hubContentMarkdown', 'importHubContent_hubContentMarkdown' - Markdown files associated with the hub content to import.
--
-- 'hubContentSearchKeywords', 'importHubContent_hubContentSearchKeywords' - The searchable keywords of the hub content.
--
-- 'hubContentVersion', 'importHubContent_hubContentVersion' - The version of the hub content to import.
--
-- 'tags', 'importHubContent_tags' - Any tags associated with the hub content.
--
-- 'hubContentName', 'importHubContent_hubContentName' - The name of the hub content to import.
--
-- 'hubContentType', 'importHubContent_hubContentType' - The type of hub content to import.
--
-- 'documentSchemaVersion', 'importHubContent_documentSchemaVersion' - The version of the hub content schema to import.
--
-- 'hubName', 'importHubContent_hubName' - The name of the hub to import content into.
--
-- 'hubContentDocument', 'importHubContent_hubContentDocument' - The hub content document that describes information about the hub
-- content such as type, associated containers, scripts, and more.
newImportHubContent ::
  -- | 'hubContentName'
  Prelude.Text ->
  -- | 'hubContentType'
  HubContentType ->
  -- | 'documentSchemaVersion'
  Prelude.Text ->
  -- | 'hubName'
  Prelude.Text ->
  -- | 'hubContentDocument'
  Prelude.Text ->
  ImportHubContent
newImportHubContent
  pHubContentName_
  pHubContentType_
  pDocumentSchemaVersion_
  pHubName_
  pHubContentDocument_ =
    ImportHubContent'
      { hubContentDescription =
          Prelude.Nothing,
        hubContentDisplayName = Prelude.Nothing,
        hubContentMarkdown = Prelude.Nothing,
        hubContentSearchKeywords = Prelude.Nothing,
        hubContentVersion = Prelude.Nothing,
        tags = Prelude.Nothing,
        hubContentName = pHubContentName_,
        hubContentType = pHubContentType_,
        documentSchemaVersion = pDocumentSchemaVersion_,
        hubName = pHubName_,
        hubContentDocument = pHubContentDocument_
      }

-- | A description of the hub content to import.
importHubContent_hubContentDescription :: Lens.Lens' ImportHubContent (Prelude.Maybe Prelude.Text)
importHubContent_hubContentDescription = Lens.lens (\ImportHubContent' {hubContentDescription} -> hubContentDescription) (\s@ImportHubContent' {} a -> s {hubContentDescription = a} :: ImportHubContent)

-- | The display name of the hub content to import.
importHubContent_hubContentDisplayName :: Lens.Lens' ImportHubContent (Prelude.Maybe Prelude.Text)
importHubContent_hubContentDisplayName = Lens.lens (\ImportHubContent' {hubContentDisplayName} -> hubContentDisplayName) (\s@ImportHubContent' {} a -> s {hubContentDisplayName = a} :: ImportHubContent)

-- | Markdown files associated with the hub content to import.
importHubContent_hubContentMarkdown :: Lens.Lens' ImportHubContent (Prelude.Maybe Prelude.Text)
importHubContent_hubContentMarkdown = Lens.lens (\ImportHubContent' {hubContentMarkdown} -> hubContentMarkdown) (\s@ImportHubContent' {} a -> s {hubContentMarkdown = a} :: ImportHubContent)

-- | The searchable keywords of the hub content.
importHubContent_hubContentSearchKeywords :: Lens.Lens' ImportHubContent (Prelude.Maybe [Prelude.Text])
importHubContent_hubContentSearchKeywords = Lens.lens (\ImportHubContent' {hubContentSearchKeywords} -> hubContentSearchKeywords) (\s@ImportHubContent' {} a -> s {hubContentSearchKeywords = a} :: ImportHubContent) Prelude.. Lens.mapping Lens.coerced

-- | The version of the hub content to import.
importHubContent_hubContentVersion :: Lens.Lens' ImportHubContent (Prelude.Maybe Prelude.Text)
importHubContent_hubContentVersion = Lens.lens (\ImportHubContent' {hubContentVersion} -> hubContentVersion) (\s@ImportHubContent' {} a -> s {hubContentVersion = a} :: ImportHubContent)

-- | Any tags associated with the hub content.
importHubContent_tags :: Lens.Lens' ImportHubContent (Prelude.Maybe [Tag])
importHubContent_tags = Lens.lens (\ImportHubContent' {tags} -> tags) (\s@ImportHubContent' {} a -> s {tags = a} :: ImportHubContent) Prelude.. Lens.mapping Lens.coerced

-- | The name of the hub content to import.
importHubContent_hubContentName :: Lens.Lens' ImportHubContent Prelude.Text
importHubContent_hubContentName = Lens.lens (\ImportHubContent' {hubContentName} -> hubContentName) (\s@ImportHubContent' {} a -> s {hubContentName = a} :: ImportHubContent)

-- | The type of hub content to import.
importHubContent_hubContentType :: Lens.Lens' ImportHubContent HubContentType
importHubContent_hubContentType = Lens.lens (\ImportHubContent' {hubContentType} -> hubContentType) (\s@ImportHubContent' {} a -> s {hubContentType = a} :: ImportHubContent)

-- | The version of the hub content schema to import.
importHubContent_documentSchemaVersion :: Lens.Lens' ImportHubContent Prelude.Text
importHubContent_documentSchemaVersion = Lens.lens (\ImportHubContent' {documentSchemaVersion} -> documentSchemaVersion) (\s@ImportHubContent' {} a -> s {documentSchemaVersion = a} :: ImportHubContent)

-- | The name of the hub to import content into.
importHubContent_hubName :: Lens.Lens' ImportHubContent Prelude.Text
importHubContent_hubName = Lens.lens (\ImportHubContent' {hubName} -> hubName) (\s@ImportHubContent' {} a -> s {hubName = a} :: ImportHubContent)

-- | The hub content document that describes information about the hub
-- content such as type, associated containers, scripts, and more.
importHubContent_hubContentDocument :: Lens.Lens' ImportHubContent Prelude.Text
importHubContent_hubContentDocument = Lens.lens (\ImportHubContent' {hubContentDocument} -> hubContentDocument) (\s@ImportHubContent' {} a -> s {hubContentDocument = a} :: ImportHubContent)

instance Core.AWSRequest ImportHubContent where
  type
    AWSResponse ImportHubContent =
      ImportHubContentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ImportHubContentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "HubArn")
            Prelude.<*> (x Data..:> "HubContentArn")
      )

instance Prelude.Hashable ImportHubContent where
  hashWithSalt _salt ImportHubContent' {..} =
    _salt `Prelude.hashWithSalt` hubContentDescription
      `Prelude.hashWithSalt` hubContentDisplayName
      `Prelude.hashWithSalt` hubContentMarkdown
      `Prelude.hashWithSalt` hubContentSearchKeywords
      `Prelude.hashWithSalt` hubContentVersion
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` hubContentName
      `Prelude.hashWithSalt` hubContentType
      `Prelude.hashWithSalt` documentSchemaVersion
      `Prelude.hashWithSalt` hubName
      `Prelude.hashWithSalt` hubContentDocument

instance Prelude.NFData ImportHubContent where
  rnf ImportHubContent' {..} =
    Prelude.rnf hubContentDescription
      `Prelude.seq` Prelude.rnf hubContentDisplayName
      `Prelude.seq` Prelude.rnf hubContentMarkdown
      `Prelude.seq` Prelude.rnf hubContentSearchKeywords
      `Prelude.seq` Prelude.rnf hubContentVersion
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf hubContentName
      `Prelude.seq` Prelude.rnf hubContentType
      `Prelude.seq` Prelude.rnf documentSchemaVersion
      `Prelude.seq` Prelude.rnf hubName
      `Prelude.seq` Prelude.rnf hubContentDocument

instance Data.ToHeaders ImportHubContent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.ImportHubContent" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ImportHubContent where
  toJSON ImportHubContent' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("HubContentDescription" Data..=)
              Prelude.<$> hubContentDescription,
            ("HubContentDisplayName" Data..=)
              Prelude.<$> hubContentDisplayName,
            ("HubContentMarkdown" Data..=)
              Prelude.<$> hubContentMarkdown,
            ("HubContentSearchKeywords" Data..=)
              Prelude.<$> hubContentSearchKeywords,
            ("HubContentVersion" Data..=)
              Prelude.<$> hubContentVersion,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("HubContentName" Data..= hubContentName),
            Prelude.Just
              ("HubContentType" Data..= hubContentType),
            Prelude.Just
              ( "DocumentSchemaVersion"
                  Data..= documentSchemaVersion
              ),
            Prelude.Just ("HubName" Data..= hubName),
            Prelude.Just
              ("HubContentDocument" Data..= hubContentDocument)
          ]
      )

instance Data.ToPath ImportHubContent where
  toPath = Prelude.const "/"

instance Data.ToQuery ImportHubContent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newImportHubContentResponse' smart constructor.
data ImportHubContentResponse = ImportHubContentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ARN of the hub that the content was imported into.
    hubArn :: Prelude.Text,
    -- | The ARN of the hub content that was imported.
    hubContentArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportHubContentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'importHubContentResponse_httpStatus' - The response's http status code.
--
-- 'hubArn', 'importHubContentResponse_hubArn' - The ARN of the hub that the content was imported into.
--
-- 'hubContentArn', 'importHubContentResponse_hubContentArn' - The ARN of the hub content that was imported.
newImportHubContentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'hubArn'
  Prelude.Text ->
  -- | 'hubContentArn'
  Prelude.Text ->
  ImportHubContentResponse
newImportHubContentResponse
  pHttpStatus_
  pHubArn_
  pHubContentArn_ =
    ImportHubContentResponse'
      { httpStatus =
          pHttpStatus_,
        hubArn = pHubArn_,
        hubContentArn = pHubContentArn_
      }

-- | The response's http status code.
importHubContentResponse_httpStatus :: Lens.Lens' ImportHubContentResponse Prelude.Int
importHubContentResponse_httpStatus = Lens.lens (\ImportHubContentResponse' {httpStatus} -> httpStatus) (\s@ImportHubContentResponse' {} a -> s {httpStatus = a} :: ImportHubContentResponse)

-- | The ARN of the hub that the content was imported into.
importHubContentResponse_hubArn :: Lens.Lens' ImportHubContentResponse Prelude.Text
importHubContentResponse_hubArn = Lens.lens (\ImportHubContentResponse' {hubArn} -> hubArn) (\s@ImportHubContentResponse' {} a -> s {hubArn = a} :: ImportHubContentResponse)

-- | The ARN of the hub content that was imported.
importHubContentResponse_hubContentArn :: Lens.Lens' ImportHubContentResponse Prelude.Text
importHubContentResponse_hubContentArn = Lens.lens (\ImportHubContentResponse' {hubContentArn} -> hubContentArn) (\s@ImportHubContentResponse' {} a -> s {hubContentArn = a} :: ImportHubContentResponse)

instance Prelude.NFData ImportHubContentResponse where
  rnf ImportHubContentResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf hubArn
      `Prelude.seq` Prelude.rnf hubContentArn
