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
-- Module      : Amazonka.DataSync.StopDiscoveryJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a running DataSync discovery job.
--
-- You can stop a discovery job anytime. A job that\'s stopped before it\'s
-- scheduled to end likely will provide you some information about your
-- on-premises storage system resources. To get recommendations for a
-- stopped job, you must use the
-- <https://docs.aws.amazon.com/datasync/latest/userguide/API_GenerateRecommendations.html GenerateRecommendations>
-- operation.
module Amazonka.DataSync.StopDiscoveryJob
  ( -- * Creating a Request
    StopDiscoveryJob (..),
    newStopDiscoveryJob,

    -- * Request Lenses
    stopDiscoveryJob_discoveryJobArn,

    -- * Destructuring the Response
    StopDiscoveryJobResponse (..),
    newStopDiscoveryJobResponse,

    -- * Response Lenses
    stopDiscoveryJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataSync.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopDiscoveryJob' smart constructor.
data StopDiscoveryJob = StopDiscoveryJob'
  { -- | Specifies the Amazon Resource Name (ARN) of the discovery job that you
    -- want to stop.
    discoveryJobArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopDiscoveryJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'discoveryJobArn', 'stopDiscoveryJob_discoveryJobArn' - Specifies the Amazon Resource Name (ARN) of the discovery job that you
-- want to stop.
newStopDiscoveryJob ::
  -- | 'discoveryJobArn'
  Prelude.Text ->
  StopDiscoveryJob
newStopDiscoveryJob pDiscoveryJobArn_ =
  StopDiscoveryJob'
    { discoveryJobArn =
        pDiscoveryJobArn_
    }

-- | Specifies the Amazon Resource Name (ARN) of the discovery job that you
-- want to stop.
stopDiscoveryJob_discoveryJobArn :: Lens.Lens' StopDiscoveryJob Prelude.Text
stopDiscoveryJob_discoveryJobArn = Lens.lens (\StopDiscoveryJob' {discoveryJobArn} -> discoveryJobArn) (\s@StopDiscoveryJob' {} a -> s {discoveryJobArn = a} :: StopDiscoveryJob)

instance Core.AWSRequest StopDiscoveryJob where
  type
    AWSResponse StopDiscoveryJob =
      StopDiscoveryJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopDiscoveryJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopDiscoveryJob where
  hashWithSalt _salt StopDiscoveryJob' {..} =
    _salt `Prelude.hashWithSalt` discoveryJobArn

instance Prelude.NFData StopDiscoveryJob where
  rnf StopDiscoveryJob' {..} =
    Prelude.rnf discoveryJobArn

instance Data.ToHeaders StopDiscoveryJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "FmrsService.StopDiscoveryJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopDiscoveryJob where
  toJSON StopDiscoveryJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("DiscoveryJobArn" Data..= discoveryJobArn)
          ]
      )

instance Data.ToPath StopDiscoveryJob where
  toPath = Prelude.const "/"

instance Data.ToQuery StopDiscoveryJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopDiscoveryJobResponse' smart constructor.
data StopDiscoveryJobResponse = StopDiscoveryJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopDiscoveryJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'stopDiscoveryJobResponse_httpStatus' - The response's http status code.
newStopDiscoveryJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopDiscoveryJobResponse
newStopDiscoveryJobResponse pHttpStatus_ =
  StopDiscoveryJobResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
stopDiscoveryJobResponse_httpStatus :: Lens.Lens' StopDiscoveryJobResponse Prelude.Int
stopDiscoveryJobResponse_httpStatus = Lens.lens (\StopDiscoveryJobResponse' {httpStatus} -> httpStatus) (\s@StopDiscoveryJobResponse' {} a -> s {httpStatus = a} :: StopDiscoveryJobResponse)

instance Prelude.NFData StopDiscoveryJobResponse where
  rnf StopDiscoveryJobResponse' {..} =
    Prelude.rnf httpStatus
