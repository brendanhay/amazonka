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
-- Module      : Network.AWS.SageMaker.DescribeTrial
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of a trial\'s properties.
module Network.AWS.SageMaker.DescribeTrial
  ( -- * Creating a Request
    DescribeTrial (..),
    newDescribeTrial,

    -- * Request Lenses
    describeTrial_trialName,

    -- * Destructuring the Response
    DescribeTrialResponse (..),
    newDescribeTrialResponse,

    -- * Response Lenses
    describeTrialResponse_trialArn,
    describeTrialResponse_metadataProperties,
    describeTrialResponse_creationTime,
    describeTrialResponse_source,
    describeTrialResponse_lastModifiedTime,
    describeTrialResponse_experimentName,
    describeTrialResponse_createdBy,
    describeTrialResponse_lastModifiedBy,
    describeTrialResponse_displayName,
    describeTrialResponse_trialName,
    describeTrialResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDescribeTrial' smart constructor.
data DescribeTrial = DescribeTrial'
  { -- | The name of the trial to describe.
    trialName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeTrial' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trialName', 'describeTrial_trialName' - The name of the trial to describe.
newDescribeTrial ::
  -- | 'trialName'
  Core.Text ->
  DescribeTrial
newDescribeTrial pTrialName_ =
  DescribeTrial' {trialName = pTrialName_}

-- | The name of the trial to describe.
describeTrial_trialName :: Lens.Lens' DescribeTrial Core.Text
describeTrial_trialName = Lens.lens (\DescribeTrial' {trialName} -> trialName) (\s@DescribeTrial' {} a -> s {trialName = a} :: DescribeTrial)

instance Core.AWSRequest DescribeTrial where
  type
    AWSResponse DescribeTrial =
      DescribeTrialResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTrialResponse'
            Core.<$> (x Core..?> "TrialArn")
            Core.<*> (x Core..?> "MetadataProperties")
            Core.<*> (x Core..?> "CreationTime")
            Core.<*> (x Core..?> "Source")
            Core.<*> (x Core..?> "LastModifiedTime")
            Core.<*> (x Core..?> "ExperimentName")
            Core.<*> (x Core..?> "CreatedBy")
            Core.<*> (x Core..?> "LastModifiedBy")
            Core.<*> (x Core..?> "DisplayName")
            Core.<*> (x Core..?> "TrialName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeTrial

instance Core.NFData DescribeTrial

instance Core.ToHeaders DescribeTrial where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.DescribeTrial" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeTrial where
  toJSON DescribeTrial' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("TrialName" Core..= trialName)]
      )

instance Core.ToPath DescribeTrial where
  toPath = Core.const "/"

instance Core.ToQuery DescribeTrial where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeTrialResponse' smart constructor.
data DescribeTrialResponse = DescribeTrialResponse'
  { -- | The Amazon Resource Name (ARN) of the trial.
    trialArn :: Core.Maybe Core.Text,
    metadataProperties :: Core.Maybe MetadataProperties,
    -- | When the trial was created.
    creationTime :: Core.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the source and, optionally, the job
    -- type.
    source :: Core.Maybe TrialSource,
    -- | When the trial was last modified.
    lastModifiedTime :: Core.Maybe Core.POSIX,
    -- | The name of the experiment the trial is part of.
    experimentName :: Core.Maybe Core.Text,
    -- | Who created the trial.
    createdBy :: Core.Maybe UserContext,
    -- | Who last modified the trial.
    lastModifiedBy :: Core.Maybe UserContext,
    -- | The name of the trial as displayed. If @DisplayName@ isn\'t specified,
    -- @TrialName@ is displayed.
    displayName :: Core.Maybe Core.Text,
    -- | The name of the trial.
    trialName :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeTrialResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trialArn', 'describeTrialResponse_trialArn' - The Amazon Resource Name (ARN) of the trial.
--
-- 'metadataProperties', 'describeTrialResponse_metadataProperties' - Undocumented member.
--
-- 'creationTime', 'describeTrialResponse_creationTime' - When the trial was created.
--
-- 'source', 'describeTrialResponse_source' - The Amazon Resource Name (ARN) of the source and, optionally, the job
-- type.
--
-- 'lastModifiedTime', 'describeTrialResponse_lastModifiedTime' - When the trial was last modified.
--
-- 'experimentName', 'describeTrialResponse_experimentName' - The name of the experiment the trial is part of.
--
-- 'createdBy', 'describeTrialResponse_createdBy' - Who created the trial.
--
-- 'lastModifiedBy', 'describeTrialResponse_lastModifiedBy' - Who last modified the trial.
--
-- 'displayName', 'describeTrialResponse_displayName' - The name of the trial as displayed. If @DisplayName@ isn\'t specified,
-- @TrialName@ is displayed.
--
-- 'trialName', 'describeTrialResponse_trialName' - The name of the trial.
--
-- 'httpStatus', 'describeTrialResponse_httpStatus' - The response's http status code.
newDescribeTrialResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeTrialResponse
newDescribeTrialResponse pHttpStatus_ =
  DescribeTrialResponse'
    { trialArn = Core.Nothing,
      metadataProperties = Core.Nothing,
      creationTime = Core.Nothing,
      source = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      experimentName = Core.Nothing,
      createdBy = Core.Nothing,
      lastModifiedBy = Core.Nothing,
      displayName = Core.Nothing,
      trialName = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the trial.
describeTrialResponse_trialArn :: Lens.Lens' DescribeTrialResponse (Core.Maybe Core.Text)
describeTrialResponse_trialArn = Lens.lens (\DescribeTrialResponse' {trialArn} -> trialArn) (\s@DescribeTrialResponse' {} a -> s {trialArn = a} :: DescribeTrialResponse)

-- | Undocumented member.
describeTrialResponse_metadataProperties :: Lens.Lens' DescribeTrialResponse (Core.Maybe MetadataProperties)
describeTrialResponse_metadataProperties = Lens.lens (\DescribeTrialResponse' {metadataProperties} -> metadataProperties) (\s@DescribeTrialResponse' {} a -> s {metadataProperties = a} :: DescribeTrialResponse)

-- | When the trial was created.
describeTrialResponse_creationTime :: Lens.Lens' DescribeTrialResponse (Core.Maybe Core.UTCTime)
describeTrialResponse_creationTime = Lens.lens (\DescribeTrialResponse' {creationTime} -> creationTime) (\s@DescribeTrialResponse' {} a -> s {creationTime = a} :: DescribeTrialResponse) Core.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the source and, optionally, the job
-- type.
describeTrialResponse_source :: Lens.Lens' DescribeTrialResponse (Core.Maybe TrialSource)
describeTrialResponse_source = Lens.lens (\DescribeTrialResponse' {source} -> source) (\s@DescribeTrialResponse' {} a -> s {source = a} :: DescribeTrialResponse)

-- | When the trial was last modified.
describeTrialResponse_lastModifiedTime :: Lens.Lens' DescribeTrialResponse (Core.Maybe Core.UTCTime)
describeTrialResponse_lastModifiedTime = Lens.lens (\DescribeTrialResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeTrialResponse' {} a -> s {lastModifiedTime = a} :: DescribeTrialResponse) Core.. Lens.mapping Core._Time

-- | The name of the experiment the trial is part of.
describeTrialResponse_experimentName :: Lens.Lens' DescribeTrialResponse (Core.Maybe Core.Text)
describeTrialResponse_experimentName = Lens.lens (\DescribeTrialResponse' {experimentName} -> experimentName) (\s@DescribeTrialResponse' {} a -> s {experimentName = a} :: DescribeTrialResponse)

-- | Who created the trial.
describeTrialResponse_createdBy :: Lens.Lens' DescribeTrialResponse (Core.Maybe UserContext)
describeTrialResponse_createdBy = Lens.lens (\DescribeTrialResponse' {createdBy} -> createdBy) (\s@DescribeTrialResponse' {} a -> s {createdBy = a} :: DescribeTrialResponse)

-- | Who last modified the trial.
describeTrialResponse_lastModifiedBy :: Lens.Lens' DescribeTrialResponse (Core.Maybe UserContext)
describeTrialResponse_lastModifiedBy = Lens.lens (\DescribeTrialResponse' {lastModifiedBy} -> lastModifiedBy) (\s@DescribeTrialResponse' {} a -> s {lastModifiedBy = a} :: DescribeTrialResponse)

-- | The name of the trial as displayed. If @DisplayName@ isn\'t specified,
-- @TrialName@ is displayed.
describeTrialResponse_displayName :: Lens.Lens' DescribeTrialResponse (Core.Maybe Core.Text)
describeTrialResponse_displayName = Lens.lens (\DescribeTrialResponse' {displayName} -> displayName) (\s@DescribeTrialResponse' {} a -> s {displayName = a} :: DescribeTrialResponse)

-- | The name of the trial.
describeTrialResponse_trialName :: Lens.Lens' DescribeTrialResponse (Core.Maybe Core.Text)
describeTrialResponse_trialName = Lens.lens (\DescribeTrialResponse' {trialName} -> trialName) (\s@DescribeTrialResponse' {} a -> s {trialName = a} :: DescribeTrialResponse)

-- | The response's http status code.
describeTrialResponse_httpStatus :: Lens.Lens' DescribeTrialResponse Core.Int
describeTrialResponse_httpStatus = Lens.lens (\DescribeTrialResponse' {httpStatus} -> httpStatus) (\s@DescribeTrialResponse' {} a -> s {httpStatus = a} :: DescribeTrialResponse)

instance Core.NFData DescribeTrialResponse
