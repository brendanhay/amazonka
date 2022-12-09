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
-- Module      : Amazonka.SageMaker.DescribeModelCard
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the content, creation time, and security configuration of an
-- Amazon SageMaker Model Card.
module Amazonka.SageMaker.DescribeModelCard
  ( -- * Creating a Request
    DescribeModelCard (..),
    newDescribeModelCard,

    -- * Request Lenses
    describeModelCard_modelCardVersion,
    describeModelCard_modelCardName,

    -- * Destructuring the Response
    DescribeModelCardResponse (..),
    newDescribeModelCardResponse,

    -- * Response Lenses
    describeModelCardResponse_lastModifiedBy,
    describeModelCardResponse_lastModifiedTime,
    describeModelCardResponse_modelCardProcessingStatus,
    describeModelCardResponse_securityConfig,
    describeModelCardResponse_httpStatus,
    describeModelCardResponse_modelCardArn,
    describeModelCardResponse_modelCardName,
    describeModelCardResponse_modelCardVersion,
    describeModelCardResponse_content,
    describeModelCardResponse_modelCardStatus,
    describeModelCardResponse_creationTime,
    describeModelCardResponse_createdBy,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDescribeModelCard' smart constructor.
data DescribeModelCard = DescribeModelCard'
  { -- | The version of the model card to describe. If a version is not provided,
    -- then the latest version of the model card is described.
    modelCardVersion :: Prelude.Maybe Prelude.Int,
    -- | The name of the model card to describe.
    modelCardName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeModelCard' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelCardVersion', 'describeModelCard_modelCardVersion' - The version of the model card to describe. If a version is not provided,
-- then the latest version of the model card is described.
--
-- 'modelCardName', 'describeModelCard_modelCardName' - The name of the model card to describe.
newDescribeModelCard ::
  -- | 'modelCardName'
  Prelude.Text ->
  DescribeModelCard
newDescribeModelCard pModelCardName_ =
  DescribeModelCard'
    { modelCardVersion =
        Prelude.Nothing,
      modelCardName = pModelCardName_
    }

-- | The version of the model card to describe. If a version is not provided,
-- then the latest version of the model card is described.
describeModelCard_modelCardVersion :: Lens.Lens' DescribeModelCard (Prelude.Maybe Prelude.Int)
describeModelCard_modelCardVersion = Lens.lens (\DescribeModelCard' {modelCardVersion} -> modelCardVersion) (\s@DescribeModelCard' {} a -> s {modelCardVersion = a} :: DescribeModelCard)

-- | The name of the model card to describe.
describeModelCard_modelCardName :: Lens.Lens' DescribeModelCard Prelude.Text
describeModelCard_modelCardName = Lens.lens (\DescribeModelCard' {modelCardName} -> modelCardName) (\s@DescribeModelCard' {} a -> s {modelCardName = a} :: DescribeModelCard)

instance Core.AWSRequest DescribeModelCard where
  type
    AWSResponse DescribeModelCard =
      DescribeModelCardResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeModelCardResponse'
            Prelude.<$> (x Data..?> "LastModifiedBy")
            Prelude.<*> (x Data..?> "LastModifiedTime")
            Prelude.<*> (x Data..?> "ModelCardProcessingStatus")
            Prelude.<*> (x Data..?> "SecurityConfig")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ModelCardArn")
            Prelude.<*> (x Data..:> "ModelCardName")
            Prelude.<*> (x Data..:> "ModelCardVersion")
            Prelude.<*> (x Data..:> "Content")
            Prelude.<*> (x Data..:> "ModelCardStatus")
            Prelude.<*> (x Data..:> "CreationTime")
            Prelude.<*> (x Data..:> "CreatedBy")
      )

instance Prelude.Hashable DescribeModelCard where
  hashWithSalt _salt DescribeModelCard' {..} =
    _salt `Prelude.hashWithSalt` modelCardVersion
      `Prelude.hashWithSalt` modelCardName

instance Prelude.NFData DescribeModelCard where
  rnf DescribeModelCard' {..} =
    Prelude.rnf modelCardVersion
      `Prelude.seq` Prelude.rnf modelCardName

instance Data.ToHeaders DescribeModelCard where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.DescribeModelCard" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeModelCard where
  toJSON DescribeModelCard' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ModelCardVersion" Data..=)
              Prelude.<$> modelCardVersion,
            Prelude.Just
              ("ModelCardName" Data..= modelCardName)
          ]
      )

instance Data.ToPath DescribeModelCard where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeModelCard where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeModelCardResponse' smart constructor.
data DescribeModelCardResponse = DescribeModelCardResponse'
  { lastModifiedBy :: Prelude.Maybe UserContext,
    -- | The date and time the model card was last modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The processing status of model card deletion. The
    -- @ModelCardProcessingStatus@ updates throughout the different deletion
    -- steps.
    --
    -- -   @DeletePending@: Model card deletion request received.
    --
    -- -   @DeleteInProgress@: Model card deletion is in progress.
    --
    -- -   @ContentDeleted@: Deleted model card content.
    --
    -- -   @ExportJobsDeleted@: Deleted all export jobs associated with the
    --     model card.
    --
    -- -   @DeleteCompleted@: Successfully deleted the model card.
    --
    -- -   @DeleteFailed@: The model card failed to delete.
    modelCardProcessingStatus :: Prelude.Maybe ModelCardProcessingStatus,
    -- | The security configuration used to protect model card content.
    securityConfig :: Prelude.Maybe ModelCardSecurityConfig,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the model card.
    modelCardArn :: Prelude.Text,
    -- | The name of the model card.
    modelCardName :: Prelude.Text,
    -- | The version of the model card.
    modelCardVersion :: Prelude.Int,
    -- | The content of the model card.
    content :: Data.Sensitive Prelude.Text,
    -- | The approval status of the model card within your organization.
    -- Different organizations might have different criteria for model card
    -- review and approval.
    --
    -- -   @Draft@: The model card is a work in progress.
    --
    -- -   @PendingReview@: The model card is pending review.
    --
    -- -   @Approved@: The model card is approved.
    --
    -- -   @Archived@: The model card is archived. No more updates should be
    --     made to the model card, but it can still be exported.
    modelCardStatus :: ModelCardStatus,
    -- | The date and time the model card was created.
    creationTime :: Data.POSIX,
    createdBy :: UserContext
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeModelCardResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedBy', 'describeModelCardResponse_lastModifiedBy' - Undocumented member.
--
-- 'lastModifiedTime', 'describeModelCardResponse_lastModifiedTime' - The date and time the model card was last modified.
--
-- 'modelCardProcessingStatus', 'describeModelCardResponse_modelCardProcessingStatus' - The processing status of model card deletion. The
-- @ModelCardProcessingStatus@ updates throughout the different deletion
-- steps.
--
-- -   @DeletePending@: Model card deletion request received.
--
-- -   @DeleteInProgress@: Model card deletion is in progress.
--
-- -   @ContentDeleted@: Deleted model card content.
--
-- -   @ExportJobsDeleted@: Deleted all export jobs associated with the
--     model card.
--
-- -   @DeleteCompleted@: Successfully deleted the model card.
--
-- -   @DeleteFailed@: The model card failed to delete.
--
-- 'securityConfig', 'describeModelCardResponse_securityConfig' - The security configuration used to protect model card content.
--
-- 'httpStatus', 'describeModelCardResponse_httpStatus' - The response's http status code.
--
-- 'modelCardArn', 'describeModelCardResponse_modelCardArn' - The Amazon Resource Name (ARN) of the model card.
--
-- 'modelCardName', 'describeModelCardResponse_modelCardName' - The name of the model card.
--
-- 'modelCardVersion', 'describeModelCardResponse_modelCardVersion' - The version of the model card.
--
-- 'content', 'describeModelCardResponse_content' - The content of the model card.
--
-- 'modelCardStatus', 'describeModelCardResponse_modelCardStatus' - The approval status of the model card within your organization.
-- Different organizations might have different criteria for model card
-- review and approval.
--
-- -   @Draft@: The model card is a work in progress.
--
-- -   @PendingReview@: The model card is pending review.
--
-- -   @Approved@: The model card is approved.
--
-- -   @Archived@: The model card is archived. No more updates should be
--     made to the model card, but it can still be exported.
--
-- 'creationTime', 'describeModelCardResponse_creationTime' - The date and time the model card was created.
--
-- 'createdBy', 'describeModelCardResponse_createdBy' - Undocumented member.
newDescribeModelCardResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'modelCardArn'
  Prelude.Text ->
  -- | 'modelCardName'
  Prelude.Text ->
  -- | 'modelCardVersion'
  Prelude.Int ->
  -- | 'content'
  Prelude.Text ->
  -- | 'modelCardStatus'
  ModelCardStatus ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'createdBy'
  UserContext ->
  DescribeModelCardResponse
newDescribeModelCardResponse
  pHttpStatus_
  pModelCardArn_
  pModelCardName_
  pModelCardVersion_
  pContent_
  pModelCardStatus_
  pCreationTime_
  pCreatedBy_ =
    DescribeModelCardResponse'
      { lastModifiedBy =
          Prelude.Nothing,
        lastModifiedTime = Prelude.Nothing,
        modelCardProcessingStatus = Prelude.Nothing,
        securityConfig = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        modelCardArn = pModelCardArn_,
        modelCardName = pModelCardName_,
        modelCardVersion = pModelCardVersion_,
        content = Data._Sensitive Lens.# pContent_,
        modelCardStatus = pModelCardStatus_,
        creationTime = Data._Time Lens.# pCreationTime_,
        createdBy = pCreatedBy_
      }

-- | Undocumented member.
describeModelCardResponse_lastModifiedBy :: Lens.Lens' DescribeModelCardResponse (Prelude.Maybe UserContext)
describeModelCardResponse_lastModifiedBy = Lens.lens (\DescribeModelCardResponse' {lastModifiedBy} -> lastModifiedBy) (\s@DescribeModelCardResponse' {} a -> s {lastModifiedBy = a} :: DescribeModelCardResponse)

-- | The date and time the model card was last modified.
describeModelCardResponse_lastModifiedTime :: Lens.Lens' DescribeModelCardResponse (Prelude.Maybe Prelude.UTCTime)
describeModelCardResponse_lastModifiedTime = Lens.lens (\DescribeModelCardResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeModelCardResponse' {} a -> s {lastModifiedTime = a} :: DescribeModelCardResponse) Prelude.. Lens.mapping Data._Time

-- | The processing status of model card deletion. The
-- @ModelCardProcessingStatus@ updates throughout the different deletion
-- steps.
--
-- -   @DeletePending@: Model card deletion request received.
--
-- -   @DeleteInProgress@: Model card deletion is in progress.
--
-- -   @ContentDeleted@: Deleted model card content.
--
-- -   @ExportJobsDeleted@: Deleted all export jobs associated with the
--     model card.
--
-- -   @DeleteCompleted@: Successfully deleted the model card.
--
-- -   @DeleteFailed@: The model card failed to delete.
describeModelCardResponse_modelCardProcessingStatus :: Lens.Lens' DescribeModelCardResponse (Prelude.Maybe ModelCardProcessingStatus)
describeModelCardResponse_modelCardProcessingStatus = Lens.lens (\DescribeModelCardResponse' {modelCardProcessingStatus} -> modelCardProcessingStatus) (\s@DescribeModelCardResponse' {} a -> s {modelCardProcessingStatus = a} :: DescribeModelCardResponse)

-- | The security configuration used to protect model card content.
describeModelCardResponse_securityConfig :: Lens.Lens' DescribeModelCardResponse (Prelude.Maybe ModelCardSecurityConfig)
describeModelCardResponse_securityConfig = Lens.lens (\DescribeModelCardResponse' {securityConfig} -> securityConfig) (\s@DescribeModelCardResponse' {} a -> s {securityConfig = a} :: DescribeModelCardResponse)

-- | The response's http status code.
describeModelCardResponse_httpStatus :: Lens.Lens' DescribeModelCardResponse Prelude.Int
describeModelCardResponse_httpStatus = Lens.lens (\DescribeModelCardResponse' {httpStatus} -> httpStatus) (\s@DescribeModelCardResponse' {} a -> s {httpStatus = a} :: DescribeModelCardResponse)

-- | The Amazon Resource Name (ARN) of the model card.
describeModelCardResponse_modelCardArn :: Lens.Lens' DescribeModelCardResponse Prelude.Text
describeModelCardResponse_modelCardArn = Lens.lens (\DescribeModelCardResponse' {modelCardArn} -> modelCardArn) (\s@DescribeModelCardResponse' {} a -> s {modelCardArn = a} :: DescribeModelCardResponse)

-- | The name of the model card.
describeModelCardResponse_modelCardName :: Lens.Lens' DescribeModelCardResponse Prelude.Text
describeModelCardResponse_modelCardName = Lens.lens (\DescribeModelCardResponse' {modelCardName} -> modelCardName) (\s@DescribeModelCardResponse' {} a -> s {modelCardName = a} :: DescribeModelCardResponse)

-- | The version of the model card.
describeModelCardResponse_modelCardVersion :: Lens.Lens' DescribeModelCardResponse Prelude.Int
describeModelCardResponse_modelCardVersion = Lens.lens (\DescribeModelCardResponse' {modelCardVersion} -> modelCardVersion) (\s@DescribeModelCardResponse' {} a -> s {modelCardVersion = a} :: DescribeModelCardResponse)

-- | The content of the model card.
describeModelCardResponse_content :: Lens.Lens' DescribeModelCardResponse Prelude.Text
describeModelCardResponse_content = Lens.lens (\DescribeModelCardResponse' {content} -> content) (\s@DescribeModelCardResponse' {} a -> s {content = a} :: DescribeModelCardResponse) Prelude.. Data._Sensitive

-- | The approval status of the model card within your organization.
-- Different organizations might have different criteria for model card
-- review and approval.
--
-- -   @Draft@: The model card is a work in progress.
--
-- -   @PendingReview@: The model card is pending review.
--
-- -   @Approved@: The model card is approved.
--
-- -   @Archived@: The model card is archived. No more updates should be
--     made to the model card, but it can still be exported.
describeModelCardResponse_modelCardStatus :: Lens.Lens' DescribeModelCardResponse ModelCardStatus
describeModelCardResponse_modelCardStatus = Lens.lens (\DescribeModelCardResponse' {modelCardStatus} -> modelCardStatus) (\s@DescribeModelCardResponse' {} a -> s {modelCardStatus = a} :: DescribeModelCardResponse)

-- | The date and time the model card was created.
describeModelCardResponse_creationTime :: Lens.Lens' DescribeModelCardResponse Prelude.UTCTime
describeModelCardResponse_creationTime = Lens.lens (\DescribeModelCardResponse' {creationTime} -> creationTime) (\s@DescribeModelCardResponse' {} a -> s {creationTime = a} :: DescribeModelCardResponse) Prelude.. Data._Time

-- | Undocumented member.
describeModelCardResponse_createdBy :: Lens.Lens' DescribeModelCardResponse UserContext
describeModelCardResponse_createdBy = Lens.lens (\DescribeModelCardResponse' {createdBy} -> createdBy) (\s@DescribeModelCardResponse' {} a -> s {createdBy = a} :: DescribeModelCardResponse)

instance Prelude.NFData DescribeModelCardResponse where
  rnf DescribeModelCardResponse' {..} =
    Prelude.rnf lastModifiedBy
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf modelCardProcessingStatus
      `Prelude.seq` Prelude.rnf securityConfig
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf modelCardArn
      `Prelude.seq` Prelude.rnf modelCardName
      `Prelude.seq` Prelude.rnf modelCardVersion
      `Prelude.seq` Prelude.rnf content
      `Prelude.seq` Prelude.rnf modelCardStatus
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf createdBy
