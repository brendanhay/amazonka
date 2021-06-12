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
-- Module      : Network.AWS.SageMaker.DescribeExperiment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of an experiment\'s properties.
module Network.AWS.SageMaker.DescribeExperiment
  ( -- * Creating a Request
    DescribeExperiment (..),
    newDescribeExperiment,

    -- * Request Lenses
    describeExperiment_experimentName,

    -- * Destructuring the Response
    DescribeExperimentResponse (..),
    newDescribeExperimentResponse,

    -- * Response Lenses
    describeExperimentResponse_experimentArn,
    describeExperimentResponse_creationTime,
    describeExperimentResponse_source,
    describeExperimentResponse_lastModifiedTime,
    describeExperimentResponse_experimentName,
    describeExperimentResponse_description,
    describeExperimentResponse_createdBy,
    describeExperimentResponse_lastModifiedBy,
    describeExperimentResponse_displayName,
    describeExperimentResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDescribeExperiment' smart constructor.
data DescribeExperiment = DescribeExperiment'
  { -- | The name of the experiment to describe.
    experimentName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeExperiment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'experimentName', 'describeExperiment_experimentName' - The name of the experiment to describe.
newDescribeExperiment ::
  -- | 'experimentName'
  Core.Text ->
  DescribeExperiment
newDescribeExperiment pExperimentName_ =
  DescribeExperiment'
    { experimentName =
        pExperimentName_
    }

-- | The name of the experiment to describe.
describeExperiment_experimentName :: Lens.Lens' DescribeExperiment Core.Text
describeExperiment_experimentName = Lens.lens (\DescribeExperiment' {experimentName} -> experimentName) (\s@DescribeExperiment' {} a -> s {experimentName = a} :: DescribeExperiment)

instance Core.AWSRequest DescribeExperiment where
  type
    AWSResponse DescribeExperiment =
      DescribeExperimentResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeExperimentResponse'
            Core.<$> (x Core..?> "ExperimentArn")
            Core.<*> (x Core..?> "CreationTime")
            Core.<*> (x Core..?> "Source")
            Core.<*> (x Core..?> "LastModifiedTime")
            Core.<*> (x Core..?> "ExperimentName")
            Core.<*> (x Core..?> "Description")
            Core.<*> (x Core..?> "CreatedBy")
            Core.<*> (x Core..?> "LastModifiedBy")
            Core.<*> (x Core..?> "DisplayName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeExperiment

instance Core.NFData DescribeExperiment

instance Core.ToHeaders DescribeExperiment where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.DescribeExperiment" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeExperiment where
  toJSON DescribeExperiment' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("ExperimentName" Core..= experimentName)
          ]
      )

instance Core.ToPath DescribeExperiment where
  toPath = Core.const "/"

instance Core.ToQuery DescribeExperiment where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeExperimentResponse' smart constructor.
data DescribeExperimentResponse = DescribeExperimentResponse'
  { -- | The Amazon Resource Name (ARN) of the experiment.
    experimentArn :: Core.Maybe Core.Text,
    -- | When the experiment was created.
    creationTime :: Core.Maybe Core.POSIX,
    -- | The ARN of the source and, optionally, the type.
    source :: Core.Maybe ExperimentSource,
    -- | When the experiment was last modified.
    lastModifiedTime :: Core.Maybe Core.POSIX,
    -- | The name of the experiment.
    experimentName :: Core.Maybe Core.Text,
    -- | The description of the experiment.
    description :: Core.Maybe Core.Text,
    -- | Who created the experiment.
    createdBy :: Core.Maybe UserContext,
    -- | Who last modified the experiment.
    lastModifiedBy :: Core.Maybe UserContext,
    -- | The name of the experiment as displayed. If @DisplayName@ isn\'t
    -- specified, @ExperimentName@ is displayed.
    displayName :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeExperimentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'experimentArn', 'describeExperimentResponse_experimentArn' - The Amazon Resource Name (ARN) of the experiment.
--
-- 'creationTime', 'describeExperimentResponse_creationTime' - When the experiment was created.
--
-- 'source', 'describeExperimentResponse_source' - The ARN of the source and, optionally, the type.
--
-- 'lastModifiedTime', 'describeExperimentResponse_lastModifiedTime' - When the experiment was last modified.
--
-- 'experimentName', 'describeExperimentResponse_experimentName' - The name of the experiment.
--
-- 'description', 'describeExperimentResponse_description' - The description of the experiment.
--
-- 'createdBy', 'describeExperimentResponse_createdBy' - Who created the experiment.
--
-- 'lastModifiedBy', 'describeExperimentResponse_lastModifiedBy' - Who last modified the experiment.
--
-- 'displayName', 'describeExperimentResponse_displayName' - The name of the experiment as displayed. If @DisplayName@ isn\'t
-- specified, @ExperimentName@ is displayed.
--
-- 'httpStatus', 'describeExperimentResponse_httpStatus' - The response's http status code.
newDescribeExperimentResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeExperimentResponse
newDescribeExperimentResponse pHttpStatus_ =
  DescribeExperimentResponse'
    { experimentArn =
        Core.Nothing,
      creationTime = Core.Nothing,
      source = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      experimentName = Core.Nothing,
      description = Core.Nothing,
      createdBy = Core.Nothing,
      lastModifiedBy = Core.Nothing,
      displayName = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the experiment.
describeExperimentResponse_experimentArn :: Lens.Lens' DescribeExperimentResponse (Core.Maybe Core.Text)
describeExperimentResponse_experimentArn = Lens.lens (\DescribeExperimentResponse' {experimentArn} -> experimentArn) (\s@DescribeExperimentResponse' {} a -> s {experimentArn = a} :: DescribeExperimentResponse)

-- | When the experiment was created.
describeExperimentResponse_creationTime :: Lens.Lens' DescribeExperimentResponse (Core.Maybe Core.UTCTime)
describeExperimentResponse_creationTime = Lens.lens (\DescribeExperimentResponse' {creationTime} -> creationTime) (\s@DescribeExperimentResponse' {} a -> s {creationTime = a} :: DescribeExperimentResponse) Core.. Lens.mapping Core._Time

-- | The ARN of the source and, optionally, the type.
describeExperimentResponse_source :: Lens.Lens' DescribeExperimentResponse (Core.Maybe ExperimentSource)
describeExperimentResponse_source = Lens.lens (\DescribeExperimentResponse' {source} -> source) (\s@DescribeExperimentResponse' {} a -> s {source = a} :: DescribeExperimentResponse)

-- | When the experiment was last modified.
describeExperimentResponse_lastModifiedTime :: Lens.Lens' DescribeExperimentResponse (Core.Maybe Core.UTCTime)
describeExperimentResponse_lastModifiedTime = Lens.lens (\DescribeExperimentResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeExperimentResponse' {} a -> s {lastModifiedTime = a} :: DescribeExperimentResponse) Core.. Lens.mapping Core._Time

-- | The name of the experiment.
describeExperimentResponse_experimentName :: Lens.Lens' DescribeExperimentResponse (Core.Maybe Core.Text)
describeExperimentResponse_experimentName = Lens.lens (\DescribeExperimentResponse' {experimentName} -> experimentName) (\s@DescribeExperimentResponse' {} a -> s {experimentName = a} :: DescribeExperimentResponse)

-- | The description of the experiment.
describeExperimentResponse_description :: Lens.Lens' DescribeExperimentResponse (Core.Maybe Core.Text)
describeExperimentResponse_description = Lens.lens (\DescribeExperimentResponse' {description} -> description) (\s@DescribeExperimentResponse' {} a -> s {description = a} :: DescribeExperimentResponse)

-- | Who created the experiment.
describeExperimentResponse_createdBy :: Lens.Lens' DescribeExperimentResponse (Core.Maybe UserContext)
describeExperimentResponse_createdBy = Lens.lens (\DescribeExperimentResponse' {createdBy} -> createdBy) (\s@DescribeExperimentResponse' {} a -> s {createdBy = a} :: DescribeExperimentResponse)

-- | Who last modified the experiment.
describeExperimentResponse_lastModifiedBy :: Lens.Lens' DescribeExperimentResponse (Core.Maybe UserContext)
describeExperimentResponse_lastModifiedBy = Lens.lens (\DescribeExperimentResponse' {lastModifiedBy} -> lastModifiedBy) (\s@DescribeExperimentResponse' {} a -> s {lastModifiedBy = a} :: DescribeExperimentResponse)

-- | The name of the experiment as displayed. If @DisplayName@ isn\'t
-- specified, @ExperimentName@ is displayed.
describeExperimentResponse_displayName :: Lens.Lens' DescribeExperimentResponse (Core.Maybe Core.Text)
describeExperimentResponse_displayName = Lens.lens (\DescribeExperimentResponse' {displayName} -> displayName) (\s@DescribeExperimentResponse' {} a -> s {displayName = a} :: DescribeExperimentResponse)

-- | The response's http status code.
describeExperimentResponse_httpStatus :: Lens.Lens' DescribeExperimentResponse Core.Int
describeExperimentResponse_httpStatus = Lens.lens (\DescribeExperimentResponse' {httpStatus} -> httpStatus) (\s@DescribeExperimentResponse' {} a -> s {httpStatus = a} :: DescribeExperimentResponse)

instance Core.NFData DescribeExperimentResponse
