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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
    describeTrialResponse_createdBy,
    describeTrialResponse_creationTime,
    describeTrialResponse_displayName,
    describeTrialResponse_experimentName,
    describeTrialResponse_lastModifiedBy,
    describeTrialResponse_lastModifiedTime,
    describeTrialResponse_metadataProperties,
    describeTrialResponse_source,
    describeTrialResponse_trialArn,
    describeTrialResponse_trialName,
    describeTrialResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTrialResponse'
            Prelude.<$> (x Data..?> "CreatedBy")
            Prelude.<*> (x Data..?> "CreationTime")
            Prelude.<*> (x Data..?> "DisplayName")
            Prelude.<*> (x Data..?> "ExperimentName")
            Prelude.<*> (x Data..?> "LastModifiedBy")
            Prelude.<*> (x Data..?> "LastModifiedTime")
            Prelude.<*> (x Data..?> "MetadataProperties")
            Prelude.<*> (x Data..?> "Source")
            Prelude.<*> (x Data..?> "TrialArn")
            Prelude.<*> (x Data..?> "TrialName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeTrial where
  hashWithSalt _salt DescribeTrial' {..} =
    _salt `Prelude.hashWithSalt` trialName

instance Prelude.NFData DescribeTrial where
  rnf DescribeTrial' {..} = Prelude.rnf trialName

instance Data.ToHeaders DescribeTrial where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.DescribeTrial" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeTrial where
  toJSON DescribeTrial' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("TrialName" Data..= trialName)]
      )

instance Data.ToPath DescribeTrial where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeTrial where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeTrialResponse' smart constructor.
data DescribeTrialResponse = DescribeTrialResponse'
  { -- | Who created the trial.
    createdBy :: Prelude.Maybe UserContext,
    -- | When the trial was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the trial as displayed. If @DisplayName@ isn\'t specified,
    -- @TrialName@ is displayed.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The name of the experiment the trial is part of.
    experimentName :: Prelude.Maybe Prelude.Text,
    -- | Who last modified the trial.
    lastModifiedBy :: Prelude.Maybe UserContext,
    -- | When the trial was last modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    metadataProperties :: Prelude.Maybe MetadataProperties,
    -- | The Amazon Resource Name (ARN) of the source and, optionally, the job
    -- type.
    source :: Prelude.Maybe TrialSource,
    -- | The Amazon Resource Name (ARN) of the trial.
    trialArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the trial.
    trialName :: Prelude.Maybe Prelude.Text,
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
-- 'createdBy', 'describeTrialResponse_createdBy' - Who created the trial.
--
-- 'creationTime', 'describeTrialResponse_creationTime' - When the trial was created.
--
-- 'displayName', 'describeTrialResponse_displayName' - The name of the trial as displayed. If @DisplayName@ isn\'t specified,
-- @TrialName@ is displayed.
--
-- 'experimentName', 'describeTrialResponse_experimentName' - The name of the experiment the trial is part of.
--
-- 'lastModifiedBy', 'describeTrialResponse_lastModifiedBy' - Who last modified the trial.
--
-- 'lastModifiedTime', 'describeTrialResponse_lastModifiedTime' - When the trial was last modified.
--
-- 'metadataProperties', 'describeTrialResponse_metadataProperties' - Undocumented member.
--
-- 'source', 'describeTrialResponse_source' - The Amazon Resource Name (ARN) of the source and, optionally, the job
-- type.
--
-- 'trialArn', 'describeTrialResponse_trialArn' - The Amazon Resource Name (ARN) of the trial.
--
-- 'trialName', 'describeTrialResponse_trialName' - The name of the trial.
--
-- 'httpStatus', 'describeTrialResponse_httpStatus' - The response's http status code.
newDescribeTrialResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeTrialResponse
newDescribeTrialResponse pHttpStatus_ =
  DescribeTrialResponse'
    { createdBy = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      displayName = Prelude.Nothing,
      experimentName = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      metadataProperties = Prelude.Nothing,
      source = Prelude.Nothing,
      trialArn = Prelude.Nothing,
      trialName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Who created the trial.
describeTrialResponse_createdBy :: Lens.Lens' DescribeTrialResponse (Prelude.Maybe UserContext)
describeTrialResponse_createdBy = Lens.lens (\DescribeTrialResponse' {createdBy} -> createdBy) (\s@DescribeTrialResponse' {} a -> s {createdBy = a} :: DescribeTrialResponse)

-- | When the trial was created.
describeTrialResponse_creationTime :: Lens.Lens' DescribeTrialResponse (Prelude.Maybe Prelude.UTCTime)
describeTrialResponse_creationTime = Lens.lens (\DescribeTrialResponse' {creationTime} -> creationTime) (\s@DescribeTrialResponse' {} a -> s {creationTime = a} :: DescribeTrialResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the trial as displayed. If @DisplayName@ isn\'t specified,
-- @TrialName@ is displayed.
describeTrialResponse_displayName :: Lens.Lens' DescribeTrialResponse (Prelude.Maybe Prelude.Text)
describeTrialResponse_displayName = Lens.lens (\DescribeTrialResponse' {displayName} -> displayName) (\s@DescribeTrialResponse' {} a -> s {displayName = a} :: DescribeTrialResponse)

-- | The name of the experiment the trial is part of.
describeTrialResponse_experimentName :: Lens.Lens' DescribeTrialResponse (Prelude.Maybe Prelude.Text)
describeTrialResponse_experimentName = Lens.lens (\DescribeTrialResponse' {experimentName} -> experimentName) (\s@DescribeTrialResponse' {} a -> s {experimentName = a} :: DescribeTrialResponse)

-- | Who last modified the trial.
describeTrialResponse_lastModifiedBy :: Lens.Lens' DescribeTrialResponse (Prelude.Maybe UserContext)
describeTrialResponse_lastModifiedBy = Lens.lens (\DescribeTrialResponse' {lastModifiedBy} -> lastModifiedBy) (\s@DescribeTrialResponse' {} a -> s {lastModifiedBy = a} :: DescribeTrialResponse)

-- | When the trial was last modified.
describeTrialResponse_lastModifiedTime :: Lens.Lens' DescribeTrialResponse (Prelude.Maybe Prelude.UTCTime)
describeTrialResponse_lastModifiedTime = Lens.lens (\DescribeTrialResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeTrialResponse' {} a -> s {lastModifiedTime = a} :: DescribeTrialResponse) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
describeTrialResponse_metadataProperties :: Lens.Lens' DescribeTrialResponse (Prelude.Maybe MetadataProperties)
describeTrialResponse_metadataProperties = Lens.lens (\DescribeTrialResponse' {metadataProperties} -> metadataProperties) (\s@DescribeTrialResponse' {} a -> s {metadataProperties = a} :: DescribeTrialResponse)

-- | The Amazon Resource Name (ARN) of the source and, optionally, the job
-- type.
describeTrialResponse_source :: Lens.Lens' DescribeTrialResponse (Prelude.Maybe TrialSource)
describeTrialResponse_source = Lens.lens (\DescribeTrialResponse' {source} -> source) (\s@DescribeTrialResponse' {} a -> s {source = a} :: DescribeTrialResponse)

-- | The Amazon Resource Name (ARN) of the trial.
describeTrialResponse_trialArn :: Lens.Lens' DescribeTrialResponse (Prelude.Maybe Prelude.Text)
describeTrialResponse_trialArn = Lens.lens (\DescribeTrialResponse' {trialArn} -> trialArn) (\s@DescribeTrialResponse' {} a -> s {trialArn = a} :: DescribeTrialResponse)

-- | The name of the trial.
describeTrialResponse_trialName :: Lens.Lens' DescribeTrialResponse (Prelude.Maybe Prelude.Text)
describeTrialResponse_trialName = Lens.lens (\DescribeTrialResponse' {trialName} -> trialName) (\s@DescribeTrialResponse' {} a -> s {trialName = a} :: DescribeTrialResponse)

-- | The response's http status code.
describeTrialResponse_httpStatus :: Lens.Lens' DescribeTrialResponse Prelude.Int
describeTrialResponse_httpStatus = Lens.lens (\DescribeTrialResponse' {httpStatus} -> httpStatus) (\s@DescribeTrialResponse' {} a -> s {httpStatus = a} :: DescribeTrialResponse)

instance Prelude.NFData DescribeTrialResponse where
  rnf DescribeTrialResponse' {..} =
    Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf experimentName
      `Prelude.seq` Prelude.rnf lastModifiedBy
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf metadataProperties
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf trialArn
      `Prelude.seq` Prelude.rnf trialName
      `Prelude.seq` Prelude.rnf httpStatus
