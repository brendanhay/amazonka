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
-- Module      : Amazonka.SageMaker.DescribeTrial
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of a trial\'s properties.
module Amazonka.SageMaker.DescribeTrial
  ( -- * Creating a Request
    DescribeTrial (..),
    newDescribeTrial,

    -- * Request Lenses
    describeTrial_trialName,

    -- * Destructuring the Response
    DescribeTrialResponse (..),
    newDescribeTrialResponse,

    -- * Response Lenses
    describeTrialResponse_creationTime,
    describeTrialResponse_metadataProperties,
    describeTrialResponse_trialArn,
    describeTrialResponse_createdBy,
    describeTrialResponse_lastModifiedTime,
    describeTrialResponse_experimentName,
    describeTrialResponse_source,
    describeTrialResponse_displayName,
    describeTrialResponse_trialName,
    describeTrialResponse_lastModifiedBy,
    describeTrialResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDescribeTrial' smart constructor.
data DescribeTrial = DescribeTrial'
  { -- | The name of the trial to describe.
    trialName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DescribeTrial
newDescribeTrial pTrialName_ =
  DescribeTrial' {trialName = pTrialName_}

-- | The name of the trial to describe.
describeTrial_trialName :: Lens.Lens' DescribeTrial Prelude.Text
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
            Prelude.<$> (x Core..?> "CreationTime")
            Prelude.<*> (x Core..?> "MetadataProperties")
            Prelude.<*> (x Core..?> "TrialArn")
            Prelude.<*> (x Core..?> "CreatedBy")
            Prelude.<*> (x Core..?> "LastModifiedTime")
            Prelude.<*> (x Core..?> "ExperimentName")
            Prelude.<*> (x Core..?> "Source")
            Prelude.<*> (x Core..?> "DisplayName")
            Prelude.<*> (x Core..?> "TrialName")
            Prelude.<*> (x Core..?> "LastModifiedBy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeTrial

instance Prelude.NFData DescribeTrial

instance Core.ToHeaders DescribeTrial where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.DescribeTrial" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeTrial where
  toJSON DescribeTrial' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("TrialName" Core..= trialName)]
      )

instance Core.ToPath DescribeTrial where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeTrial where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeTrialResponse' smart constructor.
data DescribeTrialResponse = DescribeTrialResponse'
  { -- | When the trial was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    metadataProperties :: Prelude.Maybe MetadataProperties,
    -- | The Amazon Resource Name (ARN) of the trial.
    trialArn :: Prelude.Maybe Prelude.Text,
    -- | Who created the trial.
    createdBy :: Prelude.Maybe UserContext,
    -- | When the trial was last modified.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the experiment the trial is part of.
    experimentName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the source and, optionally, the job
    -- type.
    source :: Prelude.Maybe TrialSource,
    -- | The name of the trial as displayed. If @DisplayName@ isn\'t specified,
    -- @TrialName@ is displayed.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The name of the trial.
    trialName :: Prelude.Maybe Prelude.Text,
    -- | Who last modified the trial.
    lastModifiedBy :: Prelude.Maybe UserContext,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTrialResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'describeTrialResponse_creationTime' - When the trial was created.
--
-- 'metadataProperties', 'describeTrialResponse_metadataProperties' - Undocumented member.
--
-- 'trialArn', 'describeTrialResponse_trialArn' - The Amazon Resource Name (ARN) of the trial.
--
-- 'createdBy', 'describeTrialResponse_createdBy' - Who created the trial.
--
-- 'lastModifiedTime', 'describeTrialResponse_lastModifiedTime' - When the trial was last modified.
--
-- 'experimentName', 'describeTrialResponse_experimentName' - The name of the experiment the trial is part of.
--
-- 'source', 'describeTrialResponse_source' - The Amazon Resource Name (ARN) of the source and, optionally, the job
-- type.
--
-- 'displayName', 'describeTrialResponse_displayName' - The name of the trial as displayed. If @DisplayName@ isn\'t specified,
-- @TrialName@ is displayed.
--
-- 'trialName', 'describeTrialResponse_trialName' - The name of the trial.
--
-- 'lastModifiedBy', 'describeTrialResponse_lastModifiedBy' - Who last modified the trial.
--
-- 'httpStatus', 'describeTrialResponse_httpStatus' - The response's http status code.
newDescribeTrialResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeTrialResponse
newDescribeTrialResponse pHttpStatus_ =
  DescribeTrialResponse'
    { creationTime =
        Prelude.Nothing,
      metadataProperties = Prelude.Nothing,
      trialArn = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      experimentName = Prelude.Nothing,
      source = Prelude.Nothing,
      displayName = Prelude.Nothing,
      trialName = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | When the trial was created.
describeTrialResponse_creationTime :: Lens.Lens' DescribeTrialResponse (Prelude.Maybe Prelude.UTCTime)
describeTrialResponse_creationTime = Lens.lens (\DescribeTrialResponse' {creationTime} -> creationTime) (\s@DescribeTrialResponse' {} a -> s {creationTime = a} :: DescribeTrialResponse) Prelude.. Lens.mapping Core._Time

-- | Undocumented member.
describeTrialResponse_metadataProperties :: Lens.Lens' DescribeTrialResponse (Prelude.Maybe MetadataProperties)
describeTrialResponse_metadataProperties = Lens.lens (\DescribeTrialResponse' {metadataProperties} -> metadataProperties) (\s@DescribeTrialResponse' {} a -> s {metadataProperties = a} :: DescribeTrialResponse)

-- | The Amazon Resource Name (ARN) of the trial.
describeTrialResponse_trialArn :: Lens.Lens' DescribeTrialResponse (Prelude.Maybe Prelude.Text)
describeTrialResponse_trialArn = Lens.lens (\DescribeTrialResponse' {trialArn} -> trialArn) (\s@DescribeTrialResponse' {} a -> s {trialArn = a} :: DescribeTrialResponse)

-- | Who created the trial.
describeTrialResponse_createdBy :: Lens.Lens' DescribeTrialResponse (Prelude.Maybe UserContext)
describeTrialResponse_createdBy = Lens.lens (\DescribeTrialResponse' {createdBy} -> createdBy) (\s@DescribeTrialResponse' {} a -> s {createdBy = a} :: DescribeTrialResponse)

-- | When the trial was last modified.
describeTrialResponse_lastModifiedTime :: Lens.Lens' DescribeTrialResponse (Prelude.Maybe Prelude.UTCTime)
describeTrialResponse_lastModifiedTime = Lens.lens (\DescribeTrialResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeTrialResponse' {} a -> s {lastModifiedTime = a} :: DescribeTrialResponse) Prelude.. Lens.mapping Core._Time

-- | The name of the experiment the trial is part of.
describeTrialResponse_experimentName :: Lens.Lens' DescribeTrialResponse (Prelude.Maybe Prelude.Text)
describeTrialResponse_experimentName = Lens.lens (\DescribeTrialResponse' {experimentName} -> experimentName) (\s@DescribeTrialResponse' {} a -> s {experimentName = a} :: DescribeTrialResponse)

-- | The Amazon Resource Name (ARN) of the source and, optionally, the job
-- type.
describeTrialResponse_source :: Lens.Lens' DescribeTrialResponse (Prelude.Maybe TrialSource)
describeTrialResponse_source = Lens.lens (\DescribeTrialResponse' {source} -> source) (\s@DescribeTrialResponse' {} a -> s {source = a} :: DescribeTrialResponse)

-- | The name of the trial as displayed. If @DisplayName@ isn\'t specified,
-- @TrialName@ is displayed.
describeTrialResponse_displayName :: Lens.Lens' DescribeTrialResponse (Prelude.Maybe Prelude.Text)
describeTrialResponse_displayName = Lens.lens (\DescribeTrialResponse' {displayName} -> displayName) (\s@DescribeTrialResponse' {} a -> s {displayName = a} :: DescribeTrialResponse)

-- | The name of the trial.
describeTrialResponse_trialName :: Lens.Lens' DescribeTrialResponse (Prelude.Maybe Prelude.Text)
describeTrialResponse_trialName = Lens.lens (\DescribeTrialResponse' {trialName} -> trialName) (\s@DescribeTrialResponse' {} a -> s {trialName = a} :: DescribeTrialResponse)

-- | Who last modified the trial.
describeTrialResponse_lastModifiedBy :: Lens.Lens' DescribeTrialResponse (Prelude.Maybe UserContext)
describeTrialResponse_lastModifiedBy = Lens.lens (\DescribeTrialResponse' {lastModifiedBy} -> lastModifiedBy) (\s@DescribeTrialResponse' {} a -> s {lastModifiedBy = a} :: DescribeTrialResponse)

-- | The response's http status code.
describeTrialResponse_httpStatus :: Lens.Lens' DescribeTrialResponse Prelude.Int
describeTrialResponse_httpStatus = Lens.lens (\DescribeTrialResponse' {httpStatus} -> httpStatus) (\s@DescribeTrialResponse' {} a -> s {httpStatus = a} :: DescribeTrialResponse)

instance Prelude.NFData DescribeTrialResponse
