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
-- Module      : Amazonka.SageMaker.DescribeHub
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe a hub.
--
-- Hub APIs are only callable through SageMaker Studio.
module Amazonka.SageMaker.DescribeHub
  ( -- * Creating a Request
    DescribeHub (..),
    newDescribeHub,

    -- * Request Lenses
    describeHub_hubName,

    -- * Destructuring the Response
    DescribeHubResponse (..),
    newDescribeHubResponse,

    -- * Response Lenses
    describeHubResponse_failureReason,
    describeHubResponse_hubDescription,
    describeHubResponse_hubDisplayName,
    describeHubResponse_hubSearchKeywords,
    describeHubResponse_s3StorageConfig,
    describeHubResponse_httpStatus,
    describeHubResponse_hubName,
    describeHubResponse_hubArn,
    describeHubResponse_hubStatus,
    describeHubResponse_creationTime,
    describeHubResponse_lastModifiedTime,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDescribeHub' smart constructor.
data DescribeHub = DescribeHub'
  { -- | The name of the hub to describe.
    hubName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeHub' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hubName', 'describeHub_hubName' - The name of the hub to describe.
newDescribeHub ::
  -- | 'hubName'
  Prelude.Text ->
  DescribeHub
newDescribeHub pHubName_ =
  DescribeHub' {hubName = pHubName_}

-- | The name of the hub to describe.
describeHub_hubName :: Lens.Lens' DescribeHub Prelude.Text
describeHub_hubName = Lens.lens (\DescribeHub' {hubName} -> hubName) (\s@DescribeHub' {} a -> s {hubName = a} :: DescribeHub)

instance Core.AWSRequest DescribeHub where
  type AWSResponse DescribeHub = DescribeHubResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeHubResponse'
            Prelude.<$> (x Data..?> "FailureReason")
            Prelude.<*> (x Data..?> "HubDescription")
            Prelude.<*> (x Data..?> "HubDisplayName")
            Prelude.<*> ( x
                            Data..?> "HubSearchKeywords"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "S3StorageConfig")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "HubName")
            Prelude.<*> (x Data..:> "HubArn")
            Prelude.<*> (x Data..:> "HubStatus")
            Prelude.<*> (x Data..:> "CreationTime")
            Prelude.<*> (x Data..:> "LastModifiedTime")
      )

instance Prelude.Hashable DescribeHub where
  hashWithSalt _salt DescribeHub' {..} =
    _salt `Prelude.hashWithSalt` hubName

instance Prelude.NFData DescribeHub where
  rnf DescribeHub' {..} = Prelude.rnf hubName

instance Data.ToHeaders DescribeHub where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.DescribeHub" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeHub where
  toJSON DescribeHub' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("HubName" Data..= hubName)]
      )

instance Data.ToPath DescribeHub where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeHub where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeHubResponse' smart constructor.
data DescribeHubResponse = DescribeHubResponse'
  { -- | The failure reason if importing hub content failed.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | A description of the hub.
    hubDescription :: Prelude.Maybe Prelude.Text,
    -- | The display name of the hub.
    hubDisplayName :: Prelude.Maybe Prelude.Text,
    -- | The searchable keywords for the hub.
    hubSearchKeywords :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon S3 storage configuration for the hub.
    s3StorageConfig :: Prelude.Maybe HubS3StorageConfig,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the hub.
    hubName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the hub.
    hubArn :: Prelude.Text,
    -- | The status of the hub.
    hubStatus :: HubStatus,
    -- | The date and time that the hub was created.
    creationTime :: Data.POSIX,
    -- | The date and time that the hub was last modified.
    lastModifiedTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeHubResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureReason', 'describeHubResponse_failureReason' - The failure reason if importing hub content failed.
--
-- 'hubDescription', 'describeHubResponse_hubDescription' - A description of the hub.
--
-- 'hubDisplayName', 'describeHubResponse_hubDisplayName' - The display name of the hub.
--
-- 'hubSearchKeywords', 'describeHubResponse_hubSearchKeywords' - The searchable keywords for the hub.
--
-- 's3StorageConfig', 'describeHubResponse_s3StorageConfig' - The Amazon S3 storage configuration for the hub.
--
-- 'httpStatus', 'describeHubResponse_httpStatus' - The response's http status code.
--
-- 'hubName', 'describeHubResponse_hubName' - The name of the hub.
--
-- 'hubArn', 'describeHubResponse_hubArn' - The Amazon Resource Name (ARN) of the hub.
--
-- 'hubStatus', 'describeHubResponse_hubStatus' - The status of the hub.
--
-- 'creationTime', 'describeHubResponse_creationTime' - The date and time that the hub was created.
--
-- 'lastModifiedTime', 'describeHubResponse_lastModifiedTime' - The date and time that the hub was last modified.
newDescribeHubResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'hubName'
  Prelude.Text ->
  -- | 'hubArn'
  Prelude.Text ->
  -- | 'hubStatus'
  HubStatus ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'lastModifiedTime'
  Prelude.UTCTime ->
  DescribeHubResponse
newDescribeHubResponse
  pHttpStatus_
  pHubName_
  pHubArn_
  pHubStatus_
  pCreationTime_
  pLastModifiedTime_ =
    DescribeHubResponse'
      { failureReason =
          Prelude.Nothing,
        hubDescription = Prelude.Nothing,
        hubDisplayName = Prelude.Nothing,
        hubSearchKeywords = Prelude.Nothing,
        s3StorageConfig = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        hubName = pHubName_,
        hubArn = pHubArn_,
        hubStatus = pHubStatus_,
        creationTime = Data._Time Lens.# pCreationTime_,
        lastModifiedTime =
          Data._Time Lens.# pLastModifiedTime_
      }

-- | The failure reason if importing hub content failed.
describeHubResponse_failureReason :: Lens.Lens' DescribeHubResponse (Prelude.Maybe Prelude.Text)
describeHubResponse_failureReason = Lens.lens (\DescribeHubResponse' {failureReason} -> failureReason) (\s@DescribeHubResponse' {} a -> s {failureReason = a} :: DescribeHubResponse)

-- | A description of the hub.
describeHubResponse_hubDescription :: Lens.Lens' DescribeHubResponse (Prelude.Maybe Prelude.Text)
describeHubResponse_hubDescription = Lens.lens (\DescribeHubResponse' {hubDescription} -> hubDescription) (\s@DescribeHubResponse' {} a -> s {hubDescription = a} :: DescribeHubResponse)

-- | The display name of the hub.
describeHubResponse_hubDisplayName :: Lens.Lens' DescribeHubResponse (Prelude.Maybe Prelude.Text)
describeHubResponse_hubDisplayName = Lens.lens (\DescribeHubResponse' {hubDisplayName} -> hubDisplayName) (\s@DescribeHubResponse' {} a -> s {hubDisplayName = a} :: DescribeHubResponse)

-- | The searchable keywords for the hub.
describeHubResponse_hubSearchKeywords :: Lens.Lens' DescribeHubResponse (Prelude.Maybe [Prelude.Text])
describeHubResponse_hubSearchKeywords = Lens.lens (\DescribeHubResponse' {hubSearchKeywords} -> hubSearchKeywords) (\s@DescribeHubResponse' {} a -> s {hubSearchKeywords = a} :: DescribeHubResponse) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon S3 storage configuration for the hub.
describeHubResponse_s3StorageConfig :: Lens.Lens' DescribeHubResponse (Prelude.Maybe HubS3StorageConfig)
describeHubResponse_s3StorageConfig = Lens.lens (\DescribeHubResponse' {s3StorageConfig} -> s3StorageConfig) (\s@DescribeHubResponse' {} a -> s {s3StorageConfig = a} :: DescribeHubResponse)

-- | The response's http status code.
describeHubResponse_httpStatus :: Lens.Lens' DescribeHubResponse Prelude.Int
describeHubResponse_httpStatus = Lens.lens (\DescribeHubResponse' {httpStatus} -> httpStatus) (\s@DescribeHubResponse' {} a -> s {httpStatus = a} :: DescribeHubResponse)

-- | The name of the hub.
describeHubResponse_hubName :: Lens.Lens' DescribeHubResponse Prelude.Text
describeHubResponse_hubName = Lens.lens (\DescribeHubResponse' {hubName} -> hubName) (\s@DescribeHubResponse' {} a -> s {hubName = a} :: DescribeHubResponse)

-- | The Amazon Resource Name (ARN) of the hub.
describeHubResponse_hubArn :: Lens.Lens' DescribeHubResponse Prelude.Text
describeHubResponse_hubArn = Lens.lens (\DescribeHubResponse' {hubArn} -> hubArn) (\s@DescribeHubResponse' {} a -> s {hubArn = a} :: DescribeHubResponse)

-- | The status of the hub.
describeHubResponse_hubStatus :: Lens.Lens' DescribeHubResponse HubStatus
describeHubResponse_hubStatus = Lens.lens (\DescribeHubResponse' {hubStatus} -> hubStatus) (\s@DescribeHubResponse' {} a -> s {hubStatus = a} :: DescribeHubResponse)

-- | The date and time that the hub was created.
describeHubResponse_creationTime :: Lens.Lens' DescribeHubResponse Prelude.UTCTime
describeHubResponse_creationTime = Lens.lens (\DescribeHubResponse' {creationTime} -> creationTime) (\s@DescribeHubResponse' {} a -> s {creationTime = a} :: DescribeHubResponse) Prelude.. Data._Time

-- | The date and time that the hub was last modified.
describeHubResponse_lastModifiedTime :: Lens.Lens' DescribeHubResponse Prelude.UTCTime
describeHubResponse_lastModifiedTime = Lens.lens (\DescribeHubResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeHubResponse' {} a -> s {lastModifiedTime = a} :: DescribeHubResponse) Prelude.. Data._Time

instance Prelude.NFData DescribeHubResponse where
  rnf DescribeHubResponse' {..} =
    Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf hubDescription
      `Prelude.seq` Prelude.rnf hubDisplayName
      `Prelude.seq` Prelude.rnf hubSearchKeywords
      `Prelude.seq` Prelude.rnf s3StorageConfig
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf hubName
      `Prelude.seq` Prelude.rnf hubArn
      `Prelude.seq` Prelude.rnf hubStatus
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastModifiedTime
