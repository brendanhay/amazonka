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
-- Module      : Amazonka.ConnectCampaigns.StartInstanceOnboardingJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Onboard the specific Amazon Connect instance to Connect Campaigns.
module Amazonka.ConnectCampaigns.StartInstanceOnboardingJob
  ( -- * Creating a Request
    StartInstanceOnboardingJob (..),
    newStartInstanceOnboardingJob,

    -- * Request Lenses
    startInstanceOnboardingJob_connectInstanceId,
    startInstanceOnboardingJob_encryptionConfig,

    -- * Destructuring the Response
    StartInstanceOnboardingJobResponse (..),
    newStartInstanceOnboardingJobResponse,

    -- * Response Lenses
    startInstanceOnboardingJobResponse_connectInstanceOnboardingJobStatus,
    startInstanceOnboardingJobResponse_httpStatus,
  )
where

import Amazonka.ConnectCampaigns.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request for StartInstanceOnboardingJob API.
--
-- /See:/ 'newStartInstanceOnboardingJob' smart constructor.
data StartInstanceOnboardingJob = StartInstanceOnboardingJob'
  { connectInstanceId :: Prelude.Text,
    encryptionConfig :: EncryptionConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartInstanceOnboardingJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectInstanceId', 'startInstanceOnboardingJob_connectInstanceId' - Undocumented member.
--
-- 'encryptionConfig', 'startInstanceOnboardingJob_encryptionConfig' - Undocumented member.
newStartInstanceOnboardingJob ::
  -- | 'connectInstanceId'
  Prelude.Text ->
  -- | 'encryptionConfig'
  EncryptionConfig ->
  StartInstanceOnboardingJob
newStartInstanceOnboardingJob
  pConnectInstanceId_
  pEncryptionConfig_ =
    StartInstanceOnboardingJob'
      { connectInstanceId =
          pConnectInstanceId_,
        encryptionConfig = pEncryptionConfig_
      }

-- | Undocumented member.
startInstanceOnboardingJob_connectInstanceId :: Lens.Lens' StartInstanceOnboardingJob Prelude.Text
startInstanceOnboardingJob_connectInstanceId = Lens.lens (\StartInstanceOnboardingJob' {connectInstanceId} -> connectInstanceId) (\s@StartInstanceOnboardingJob' {} a -> s {connectInstanceId = a} :: StartInstanceOnboardingJob)

-- | Undocumented member.
startInstanceOnboardingJob_encryptionConfig :: Lens.Lens' StartInstanceOnboardingJob EncryptionConfig
startInstanceOnboardingJob_encryptionConfig = Lens.lens (\StartInstanceOnboardingJob' {encryptionConfig} -> encryptionConfig) (\s@StartInstanceOnboardingJob' {} a -> s {encryptionConfig = a} :: StartInstanceOnboardingJob)

instance Core.AWSRequest StartInstanceOnboardingJob where
  type
    AWSResponse StartInstanceOnboardingJob =
      StartInstanceOnboardingJobResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartInstanceOnboardingJobResponse'
            Prelude.<$> (x Data..?> "connectInstanceOnboardingJobStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartInstanceOnboardingJob where
  hashWithSalt _salt StartInstanceOnboardingJob' {..} =
    _salt
      `Prelude.hashWithSalt` connectInstanceId
      `Prelude.hashWithSalt` encryptionConfig

instance Prelude.NFData StartInstanceOnboardingJob where
  rnf StartInstanceOnboardingJob' {..} =
    Prelude.rnf connectInstanceId `Prelude.seq`
      Prelude.rnf encryptionConfig

instance Data.ToHeaders StartInstanceOnboardingJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartInstanceOnboardingJob where
  toJSON StartInstanceOnboardingJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("encryptionConfig" Data..= encryptionConfig)
          ]
      )

instance Data.ToPath StartInstanceOnboardingJob where
  toPath StartInstanceOnboardingJob' {..} =
    Prelude.mconcat
      [ "/connect-instance/",
        Data.toBS connectInstanceId,
        "/onboarding"
      ]

instance Data.ToQuery StartInstanceOnboardingJob where
  toQuery = Prelude.const Prelude.mempty

-- | The response for StartInstanceOnboardingJob API.
--
-- /See:/ 'newStartInstanceOnboardingJobResponse' smart constructor.
data StartInstanceOnboardingJobResponse = StartInstanceOnboardingJobResponse'
  { connectInstanceOnboardingJobStatus :: Prelude.Maybe InstanceOnboardingJobStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartInstanceOnboardingJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectInstanceOnboardingJobStatus', 'startInstanceOnboardingJobResponse_connectInstanceOnboardingJobStatus' - Undocumented member.
--
-- 'httpStatus', 'startInstanceOnboardingJobResponse_httpStatus' - The response's http status code.
newStartInstanceOnboardingJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartInstanceOnboardingJobResponse
newStartInstanceOnboardingJobResponse pHttpStatus_ =
  StartInstanceOnboardingJobResponse'
    { connectInstanceOnboardingJobStatus =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
startInstanceOnboardingJobResponse_connectInstanceOnboardingJobStatus :: Lens.Lens' StartInstanceOnboardingJobResponse (Prelude.Maybe InstanceOnboardingJobStatus)
startInstanceOnboardingJobResponse_connectInstanceOnboardingJobStatus = Lens.lens (\StartInstanceOnboardingJobResponse' {connectInstanceOnboardingJobStatus} -> connectInstanceOnboardingJobStatus) (\s@StartInstanceOnboardingJobResponse' {} a -> s {connectInstanceOnboardingJobStatus = a} :: StartInstanceOnboardingJobResponse)

-- | The response's http status code.
startInstanceOnboardingJobResponse_httpStatus :: Lens.Lens' StartInstanceOnboardingJobResponse Prelude.Int
startInstanceOnboardingJobResponse_httpStatus = Lens.lens (\StartInstanceOnboardingJobResponse' {httpStatus} -> httpStatus) (\s@StartInstanceOnboardingJobResponse' {} a -> s {httpStatus = a} :: StartInstanceOnboardingJobResponse)

instance
  Prelude.NFData
    StartInstanceOnboardingJobResponse
  where
  rnf StartInstanceOnboardingJobResponse' {..} =
    Prelude.rnf connectInstanceOnboardingJobStatus `Prelude.seq`
      Prelude.rnf httpStatus
