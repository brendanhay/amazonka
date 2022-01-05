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
-- Module      : Amazonka.SageMaker.DescribeExperiment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of an experiment\'s properties.
module Amazonka.SageMaker.DescribeExperiment
  ( -- * Creating a Request
    DescribeExperiment (..),
    newDescribeExperiment,

    -- * Request Lenses
    describeExperiment_experimentName,

    -- * Destructuring the Response
    DescribeExperimentResponse (..),
    newDescribeExperimentResponse,

    -- * Response Lenses
    describeExperimentResponse_creationTime,
    describeExperimentResponse_createdBy,
    describeExperimentResponse_lastModifiedTime,
    describeExperimentResponse_experimentName,
    describeExperimentResponse_experimentArn,
    describeExperimentResponse_source,
    describeExperimentResponse_displayName,
    describeExperimentResponse_lastModifiedBy,
    describeExperimentResponse_description,
    describeExperimentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDescribeExperiment' smart constructor.
data DescribeExperiment = DescribeExperiment'
  { -- | The name of the experiment to describe.
    experimentName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DescribeExperiment
newDescribeExperiment pExperimentName_ =
  DescribeExperiment'
    { experimentName =
        pExperimentName_
    }

-- | The name of the experiment to describe.
describeExperiment_experimentName :: Lens.Lens' DescribeExperiment Prelude.Text
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
            Prelude.<$> (x Core..?> "CreationTime")
            Prelude.<*> (x Core..?> "CreatedBy")
            Prelude.<*> (x Core..?> "LastModifiedTime")
            Prelude.<*> (x Core..?> "ExperimentName")
            Prelude.<*> (x Core..?> "ExperimentArn")
            Prelude.<*> (x Core..?> "Source")
            Prelude.<*> (x Core..?> "DisplayName")
            Prelude.<*> (x Core..?> "LastModifiedBy")
            Prelude.<*> (x Core..?> "Description")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeExperiment where
  hashWithSalt _salt DescribeExperiment' {..} =
    _salt `Prelude.hashWithSalt` experimentName

instance Prelude.NFData DescribeExperiment where
  rnf DescribeExperiment' {..} =
    Prelude.rnf experimentName

instance Core.ToHeaders DescribeExperiment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.DescribeExperiment" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeExperiment where
  toJSON DescribeExperiment' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ExperimentName" Core..= experimentName)
          ]
      )

instance Core.ToPath DescribeExperiment where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeExperiment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeExperimentResponse' smart constructor.
data DescribeExperimentResponse = DescribeExperimentResponse'
  { -- | When the experiment was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | Who created the experiment.
    createdBy :: Prelude.Maybe UserContext,
    -- | When the experiment was last modified.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the experiment.
    experimentName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the experiment.
    experimentArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the source and, optionally, the type.
    source :: Prelude.Maybe ExperimentSource,
    -- | The name of the experiment as displayed. If @DisplayName@ isn\'t
    -- specified, @ExperimentName@ is displayed.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | Who last modified the experiment.
    lastModifiedBy :: Prelude.Maybe UserContext,
    -- | The description of the experiment.
    description :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeExperimentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'describeExperimentResponse_creationTime' - When the experiment was created.
--
-- 'createdBy', 'describeExperimentResponse_createdBy' - Who created the experiment.
--
-- 'lastModifiedTime', 'describeExperimentResponse_lastModifiedTime' - When the experiment was last modified.
--
-- 'experimentName', 'describeExperimentResponse_experimentName' - The name of the experiment.
--
-- 'experimentArn', 'describeExperimentResponse_experimentArn' - The Amazon Resource Name (ARN) of the experiment.
--
-- 'source', 'describeExperimentResponse_source' - The ARN of the source and, optionally, the type.
--
-- 'displayName', 'describeExperimentResponse_displayName' - The name of the experiment as displayed. If @DisplayName@ isn\'t
-- specified, @ExperimentName@ is displayed.
--
-- 'lastModifiedBy', 'describeExperimentResponse_lastModifiedBy' - Who last modified the experiment.
--
-- 'description', 'describeExperimentResponse_description' - The description of the experiment.
--
-- 'httpStatus', 'describeExperimentResponse_httpStatus' - The response's http status code.
newDescribeExperimentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeExperimentResponse
newDescribeExperimentResponse pHttpStatus_ =
  DescribeExperimentResponse'
    { creationTime =
        Prelude.Nothing,
      createdBy = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      experimentName = Prelude.Nothing,
      experimentArn = Prelude.Nothing,
      source = Prelude.Nothing,
      displayName = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      description = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | When the experiment was created.
describeExperimentResponse_creationTime :: Lens.Lens' DescribeExperimentResponse (Prelude.Maybe Prelude.UTCTime)
describeExperimentResponse_creationTime = Lens.lens (\DescribeExperimentResponse' {creationTime} -> creationTime) (\s@DescribeExperimentResponse' {} a -> s {creationTime = a} :: DescribeExperimentResponse) Prelude.. Lens.mapping Core._Time

-- | Who created the experiment.
describeExperimentResponse_createdBy :: Lens.Lens' DescribeExperimentResponse (Prelude.Maybe UserContext)
describeExperimentResponse_createdBy = Lens.lens (\DescribeExperimentResponse' {createdBy} -> createdBy) (\s@DescribeExperimentResponse' {} a -> s {createdBy = a} :: DescribeExperimentResponse)

-- | When the experiment was last modified.
describeExperimentResponse_lastModifiedTime :: Lens.Lens' DescribeExperimentResponse (Prelude.Maybe Prelude.UTCTime)
describeExperimentResponse_lastModifiedTime = Lens.lens (\DescribeExperimentResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeExperimentResponse' {} a -> s {lastModifiedTime = a} :: DescribeExperimentResponse) Prelude.. Lens.mapping Core._Time

-- | The name of the experiment.
describeExperimentResponse_experimentName :: Lens.Lens' DescribeExperimentResponse (Prelude.Maybe Prelude.Text)
describeExperimentResponse_experimentName = Lens.lens (\DescribeExperimentResponse' {experimentName} -> experimentName) (\s@DescribeExperimentResponse' {} a -> s {experimentName = a} :: DescribeExperimentResponse)

-- | The Amazon Resource Name (ARN) of the experiment.
describeExperimentResponse_experimentArn :: Lens.Lens' DescribeExperimentResponse (Prelude.Maybe Prelude.Text)
describeExperimentResponse_experimentArn = Lens.lens (\DescribeExperimentResponse' {experimentArn} -> experimentArn) (\s@DescribeExperimentResponse' {} a -> s {experimentArn = a} :: DescribeExperimentResponse)

-- | The ARN of the source and, optionally, the type.
describeExperimentResponse_source :: Lens.Lens' DescribeExperimentResponse (Prelude.Maybe ExperimentSource)
describeExperimentResponse_source = Lens.lens (\DescribeExperimentResponse' {source} -> source) (\s@DescribeExperimentResponse' {} a -> s {source = a} :: DescribeExperimentResponse)

-- | The name of the experiment as displayed. If @DisplayName@ isn\'t
-- specified, @ExperimentName@ is displayed.
describeExperimentResponse_displayName :: Lens.Lens' DescribeExperimentResponse (Prelude.Maybe Prelude.Text)
describeExperimentResponse_displayName = Lens.lens (\DescribeExperimentResponse' {displayName} -> displayName) (\s@DescribeExperimentResponse' {} a -> s {displayName = a} :: DescribeExperimentResponse)

-- | Who last modified the experiment.
describeExperimentResponse_lastModifiedBy :: Lens.Lens' DescribeExperimentResponse (Prelude.Maybe UserContext)
describeExperimentResponse_lastModifiedBy = Lens.lens (\DescribeExperimentResponse' {lastModifiedBy} -> lastModifiedBy) (\s@DescribeExperimentResponse' {} a -> s {lastModifiedBy = a} :: DescribeExperimentResponse)

-- | The description of the experiment.
describeExperimentResponse_description :: Lens.Lens' DescribeExperimentResponse (Prelude.Maybe Prelude.Text)
describeExperimentResponse_description = Lens.lens (\DescribeExperimentResponse' {description} -> description) (\s@DescribeExperimentResponse' {} a -> s {description = a} :: DescribeExperimentResponse)

-- | The response's http status code.
describeExperimentResponse_httpStatus :: Lens.Lens' DescribeExperimentResponse Prelude.Int
describeExperimentResponse_httpStatus = Lens.lens (\DescribeExperimentResponse' {httpStatus} -> httpStatus) (\s@DescribeExperimentResponse' {} a -> s {httpStatus = a} :: DescribeExperimentResponse)

instance Prelude.NFData DescribeExperimentResponse where
  rnf DescribeExperimentResponse' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf experimentName
      `Prelude.seq` Prelude.rnf experimentArn
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf lastModifiedBy
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf httpStatus
