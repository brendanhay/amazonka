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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDescribeTrial' smart constructor.
data DescribeTrial = DescribeTrial'
  { -- | The name of the trial to describe.
    trialName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DescribeTrial where
  type Rs DescribeTrial = DescribeTrialResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTrialResponse'
            Prelude.<$> (x Prelude..?> "TrialArn")
            Prelude.<*> (x Prelude..?> "MetadataProperties")
            Prelude.<*> (x Prelude..?> "CreationTime")
            Prelude.<*> (x Prelude..?> "Source")
            Prelude.<*> (x Prelude..?> "LastModifiedTime")
            Prelude.<*> (x Prelude..?> "ExperimentName")
            Prelude.<*> (x Prelude..?> "CreatedBy")
            Prelude.<*> (x Prelude..?> "LastModifiedBy")
            Prelude.<*> (x Prelude..?> "DisplayName")
            Prelude.<*> (x Prelude..?> "TrialName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeTrial

instance Prelude.NFData DescribeTrial

instance Prelude.ToHeaders DescribeTrial where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("SageMaker.DescribeTrial" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeTrial where
  toJSON DescribeTrial' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("TrialName" Prelude..= trialName)]
      )

instance Prelude.ToPath DescribeTrial where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeTrial where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeTrialResponse' smart constructor.
data DescribeTrialResponse = DescribeTrialResponse'
  { -- | The Amazon Resource Name (ARN) of the trial.
    trialArn :: Prelude.Maybe Prelude.Text,
    metadataProperties :: Prelude.Maybe MetadataProperties,
    -- | When the trial was created.
    creationTime :: Prelude.Maybe Prelude.POSIX,
    -- | The Amazon Resource Name (ARN) of the source and, optionally, the job
    -- type.
    source :: Prelude.Maybe TrialSource,
    -- | When the trial was last modified.
    lastModifiedTime :: Prelude.Maybe Prelude.POSIX,
    -- | The name of the experiment the trial is part of.
    experimentName :: Prelude.Maybe Prelude.Text,
    -- | Who created the trial.
    createdBy :: Prelude.Maybe UserContext,
    -- | Who last modified the trial.
    lastModifiedBy :: Prelude.Maybe UserContext,
    -- | The name of the trial as displayed. If @DisplayName@ isn\'t specified,
    -- @TrialName@ is displayed.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The name of the trial.
    trialName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DescribeTrialResponse
newDescribeTrialResponse pHttpStatus_ =
  DescribeTrialResponse'
    { trialArn = Prelude.Nothing,
      metadataProperties = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      source = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      experimentName = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      displayName = Prelude.Nothing,
      trialName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the trial.
describeTrialResponse_trialArn :: Lens.Lens' DescribeTrialResponse (Prelude.Maybe Prelude.Text)
describeTrialResponse_trialArn = Lens.lens (\DescribeTrialResponse' {trialArn} -> trialArn) (\s@DescribeTrialResponse' {} a -> s {trialArn = a} :: DescribeTrialResponse)

-- | Undocumented member.
describeTrialResponse_metadataProperties :: Lens.Lens' DescribeTrialResponse (Prelude.Maybe MetadataProperties)
describeTrialResponse_metadataProperties = Lens.lens (\DescribeTrialResponse' {metadataProperties} -> metadataProperties) (\s@DescribeTrialResponse' {} a -> s {metadataProperties = a} :: DescribeTrialResponse)

-- | When the trial was created.
describeTrialResponse_creationTime :: Lens.Lens' DescribeTrialResponse (Prelude.Maybe Prelude.UTCTime)
describeTrialResponse_creationTime = Lens.lens (\DescribeTrialResponse' {creationTime} -> creationTime) (\s@DescribeTrialResponse' {} a -> s {creationTime = a} :: DescribeTrialResponse) Prelude.. Lens.mapping Prelude._Time

-- | The Amazon Resource Name (ARN) of the source and, optionally, the job
-- type.
describeTrialResponse_source :: Lens.Lens' DescribeTrialResponse (Prelude.Maybe TrialSource)
describeTrialResponse_source = Lens.lens (\DescribeTrialResponse' {source} -> source) (\s@DescribeTrialResponse' {} a -> s {source = a} :: DescribeTrialResponse)

-- | When the trial was last modified.
describeTrialResponse_lastModifiedTime :: Lens.Lens' DescribeTrialResponse (Prelude.Maybe Prelude.UTCTime)
describeTrialResponse_lastModifiedTime = Lens.lens (\DescribeTrialResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeTrialResponse' {} a -> s {lastModifiedTime = a} :: DescribeTrialResponse) Prelude.. Lens.mapping Prelude._Time

-- | The name of the experiment the trial is part of.
describeTrialResponse_experimentName :: Lens.Lens' DescribeTrialResponse (Prelude.Maybe Prelude.Text)
describeTrialResponse_experimentName = Lens.lens (\DescribeTrialResponse' {experimentName} -> experimentName) (\s@DescribeTrialResponse' {} a -> s {experimentName = a} :: DescribeTrialResponse)

-- | Who created the trial.
describeTrialResponse_createdBy :: Lens.Lens' DescribeTrialResponse (Prelude.Maybe UserContext)
describeTrialResponse_createdBy = Lens.lens (\DescribeTrialResponse' {createdBy} -> createdBy) (\s@DescribeTrialResponse' {} a -> s {createdBy = a} :: DescribeTrialResponse)

-- | Who last modified the trial.
describeTrialResponse_lastModifiedBy :: Lens.Lens' DescribeTrialResponse (Prelude.Maybe UserContext)
describeTrialResponse_lastModifiedBy = Lens.lens (\DescribeTrialResponse' {lastModifiedBy} -> lastModifiedBy) (\s@DescribeTrialResponse' {} a -> s {lastModifiedBy = a} :: DescribeTrialResponse)

-- | The name of the trial as displayed. If @DisplayName@ isn\'t specified,
-- @TrialName@ is displayed.
describeTrialResponse_displayName :: Lens.Lens' DescribeTrialResponse (Prelude.Maybe Prelude.Text)
describeTrialResponse_displayName = Lens.lens (\DescribeTrialResponse' {displayName} -> displayName) (\s@DescribeTrialResponse' {} a -> s {displayName = a} :: DescribeTrialResponse)

-- | The name of the trial.
describeTrialResponse_trialName :: Lens.Lens' DescribeTrialResponse (Prelude.Maybe Prelude.Text)
describeTrialResponse_trialName = Lens.lens (\DescribeTrialResponse' {trialName} -> trialName) (\s@DescribeTrialResponse' {} a -> s {trialName = a} :: DescribeTrialResponse)

-- | The response's http status code.
describeTrialResponse_httpStatus :: Lens.Lens' DescribeTrialResponse Prelude.Int
describeTrialResponse_httpStatus = Lens.lens (\DescribeTrialResponse' {httpStatus} -> httpStatus) (\s@DescribeTrialResponse' {} a -> s {httpStatus = a} :: DescribeTrialResponse)

instance Prelude.NFData DescribeTrialResponse
