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
-- Module      : Amazonka.DataSync.StartDiscoveryJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Runs a DataSync discovery job on your on-premises storage system. If you
-- haven\'t added the storage system to DataSync Discovery yet, do this
-- first by using the
-- <https://docs.aws.amazon.com/datasync/latest/userguide/API_AddStorageSystem.html AddStorageSystem>
-- operation.
module Amazonka.DataSync.StartDiscoveryJob
  ( -- * Creating a Request
    StartDiscoveryJob (..),
    newStartDiscoveryJob,

    -- * Request Lenses
    startDiscoveryJob_tags,
    startDiscoveryJob_storageSystemArn,
    startDiscoveryJob_collectionDurationMinutes,
    startDiscoveryJob_clientToken,

    -- * Destructuring the Response
    StartDiscoveryJobResponse (..),
    newStartDiscoveryJobResponse,

    -- * Response Lenses
    startDiscoveryJobResponse_discoveryJobArn,
    startDiscoveryJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataSync.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartDiscoveryJob' smart constructor.
data StartDiscoveryJob = StartDiscoveryJob'
  { -- | Specifies labels that help you categorize, filter, and search for your
    -- Amazon Web Services resources.
    tags :: Prelude.Maybe [TagListEntry],
    -- | Specifies the Amazon Resource Name (ARN) of the on-premises storage
    -- system that you want to run the discovery job on.
    storageSystemArn :: Prelude.Text,
    -- | Specifies in minutes how long you want the discovery job to run.
    --
    -- For more accurate recommendations, we recommend a duration of at least
    -- 14 days. Longer durations allow time to collect a sufficient number of
    -- data points and provide a realistic representation of storage
    -- performance and utilization.
    collectionDurationMinutes :: Prelude.Natural,
    -- | Specifies a client token to make sure requests with this API operation
    -- are idempotent. If you don\'t specify a client token, DataSync generates
    -- one for you automatically.
    clientToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartDiscoveryJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'startDiscoveryJob_tags' - Specifies labels that help you categorize, filter, and search for your
-- Amazon Web Services resources.
--
-- 'storageSystemArn', 'startDiscoveryJob_storageSystemArn' - Specifies the Amazon Resource Name (ARN) of the on-premises storage
-- system that you want to run the discovery job on.
--
-- 'collectionDurationMinutes', 'startDiscoveryJob_collectionDurationMinutes' - Specifies in minutes how long you want the discovery job to run.
--
-- For more accurate recommendations, we recommend a duration of at least
-- 14 days. Longer durations allow time to collect a sufficient number of
-- data points and provide a realistic representation of storage
-- performance and utilization.
--
-- 'clientToken', 'startDiscoveryJob_clientToken' - Specifies a client token to make sure requests with this API operation
-- are idempotent. If you don\'t specify a client token, DataSync generates
-- one for you automatically.
newStartDiscoveryJob ::
  -- | 'storageSystemArn'
  Prelude.Text ->
  -- | 'collectionDurationMinutes'
  Prelude.Natural ->
  -- | 'clientToken'
  Prelude.Text ->
  StartDiscoveryJob
newStartDiscoveryJob
  pStorageSystemArn_
  pCollectionDurationMinutes_
  pClientToken_ =
    StartDiscoveryJob'
      { tags = Prelude.Nothing,
        storageSystemArn = pStorageSystemArn_,
        collectionDurationMinutes =
          pCollectionDurationMinutes_,
        clientToken = pClientToken_
      }

-- | Specifies labels that help you categorize, filter, and search for your
-- Amazon Web Services resources.
startDiscoveryJob_tags :: Lens.Lens' StartDiscoveryJob (Prelude.Maybe [TagListEntry])
startDiscoveryJob_tags = Lens.lens (\StartDiscoveryJob' {tags} -> tags) (\s@StartDiscoveryJob' {} a -> s {tags = a} :: StartDiscoveryJob) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the Amazon Resource Name (ARN) of the on-premises storage
-- system that you want to run the discovery job on.
startDiscoveryJob_storageSystemArn :: Lens.Lens' StartDiscoveryJob Prelude.Text
startDiscoveryJob_storageSystemArn = Lens.lens (\StartDiscoveryJob' {storageSystemArn} -> storageSystemArn) (\s@StartDiscoveryJob' {} a -> s {storageSystemArn = a} :: StartDiscoveryJob)

-- | Specifies in minutes how long you want the discovery job to run.
--
-- For more accurate recommendations, we recommend a duration of at least
-- 14 days. Longer durations allow time to collect a sufficient number of
-- data points and provide a realistic representation of storage
-- performance and utilization.
startDiscoveryJob_collectionDurationMinutes :: Lens.Lens' StartDiscoveryJob Prelude.Natural
startDiscoveryJob_collectionDurationMinutes = Lens.lens (\StartDiscoveryJob' {collectionDurationMinutes} -> collectionDurationMinutes) (\s@StartDiscoveryJob' {} a -> s {collectionDurationMinutes = a} :: StartDiscoveryJob)

-- | Specifies a client token to make sure requests with this API operation
-- are idempotent. If you don\'t specify a client token, DataSync generates
-- one for you automatically.
startDiscoveryJob_clientToken :: Lens.Lens' StartDiscoveryJob Prelude.Text
startDiscoveryJob_clientToken = Lens.lens (\StartDiscoveryJob' {clientToken} -> clientToken) (\s@StartDiscoveryJob' {} a -> s {clientToken = a} :: StartDiscoveryJob)

instance Core.AWSRequest StartDiscoveryJob where
  type
    AWSResponse StartDiscoveryJob =
      StartDiscoveryJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartDiscoveryJobResponse'
            Prelude.<$> (x Data..?> "DiscoveryJobArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartDiscoveryJob where
  hashWithSalt _salt StartDiscoveryJob' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` storageSystemArn
      `Prelude.hashWithSalt` collectionDurationMinutes
      `Prelude.hashWithSalt` clientToken

instance Prelude.NFData StartDiscoveryJob where
  rnf StartDiscoveryJob' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf storageSystemArn
      `Prelude.seq` Prelude.rnf collectionDurationMinutes
      `Prelude.seq` Prelude.rnf clientToken

instance Data.ToHeaders StartDiscoveryJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "FmrsService.StartDiscoveryJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartDiscoveryJob where
  toJSON StartDiscoveryJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("StorageSystemArn" Data..= storageSystemArn),
            Prelude.Just
              ( "CollectionDurationMinutes"
                  Data..= collectionDurationMinutes
              ),
            Prelude.Just ("ClientToken" Data..= clientToken)
          ]
      )

instance Data.ToPath StartDiscoveryJob where
  toPath = Prelude.const "/"

instance Data.ToQuery StartDiscoveryJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartDiscoveryJobResponse' smart constructor.
data StartDiscoveryJobResponse = StartDiscoveryJobResponse'
  { -- | The ARN of the discovery job that you started.
    discoveryJobArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartDiscoveryJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'discoveryJobArn', 'startDiscoveryJobResponse_discoveryJobArn' - The ARN of the discovery job that you started.
--
-- 'httpStatus', 'startDiscoveryJobResponse_httpStatus' - The response's http status code.
newStartDiscoveryJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartDiscoveryJobResponse
newStartDiscoveryJobResponse pHttpStatus_ =
  StartDiscoveryJobResponse'
    { discoveryJobArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the discovery job that you started.
startDiscoveryJobResponse_discoveryJobArn :: Lens.Lens' StartDiscoveryJobResponse (Prelude.Maybe Prelude.Text)
startDiscoveryJobResponse_discoveryJobArn = Lens.lens (\StartDiscoveryJobResponse' {discoveryJobArn} -> discoveryJobArn) (\s@StartDiscoveryJobResponse' {} a -> s {discoveryJobArn = a} :: StartDiscoveryJobResponse)

-- | The response's http status code.
startDiscoveryJobResponse_httpStatus :: Lens.Lens' StartDiscoveryJobResponse Prelude.Int
startDiscoveryJobResponse_httpStatus = Lens.lens (\StartDiscoveryJobResponse' {httpStatus} -> httpStatus) (\s@StartDiscoveryJobResponse' {} a -> s {httpStatus = a} :: StartDiscoveryJobResponse)

instance Prelude.NFData StartDiscoveryJobResponse where
  rnf StartDiscoveryJobResponse' {..} =
    Prelude.rnf discoveryJobArn
      `Prelude.seq` Prelude.rnf httpStatus
