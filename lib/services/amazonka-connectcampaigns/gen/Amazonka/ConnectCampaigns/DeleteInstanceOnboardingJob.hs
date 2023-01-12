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
-- Module      : Amazonka.ConnectCampaigns.DeleteInstanceOnboardingJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete the Connect Campaigns onboarding job for the specified Amazon
-- Connect instance.
module Amazonka.ConnectCampaigns.DeleteInstanceOnboardingJob
  ( -- * Creating a Request
    DeleteInstanceOnboardingJob (..),
    newDeleteInstanceOnboardingJob,

    -- * Request Lenses
    deleteInstanceOnboardingJob_connectInstanceId,

    -- * Destructuring the Response
    DeleteInstanceOnboardingJobResponse (..),
    newDeleteInstanceOnboardingJobResponse,
  )
where

import Amazonka.ConnectCampaigns.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request for DeleteInstanceOnboardingJob API.
--
-- /See:/ 'newDeleteInstanceOnboardingJob' smart constructor.
data DeleteInstanceOnboardingJob = DeleteInstanceOnboardingJob'
  { connectInstanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteInstanceOnboardingJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectInstanceId', 'deleteInstanceOnboardingJob_connectInstanceId' - Undocumented member.
newDeleteInstanceOnboardingJob ::
  -- | 'connectInstanceId'
  Prelude.Text ->
  DeleteInstanceOnboardingJob
newDeleteInstanceOnboardingJob pConnectInstanceId_ =
  DeleteInstanceOnboardingJob'
    { connectInstanceId =
        pConnectInstanceId_
    }

-- | Undocumented member.
deleteInstanceOnboardingJob_connectInstanceId :: Lens.Lens' DeleteInstanceOnboardingJob Prelude.Text
deleteInstanceOnboardingJob_connectInstanceId = Lens.lens (\DeleteInstanceOnboardingJob' {connectInstanceId} -> connectInstanceId) (\s@DeleteInstanceOnboardingJob' {} a -> s {connectInstanceId = a} :: DeleteInstanceOnboardingJob)

instance Core.AWSRequest DeleteInstanceOnboardingJob where
  type
    AWSResponse DeleteInstanceOnboardingJob =
      DeleteInstanceOnboardingJobResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DeleteInstanceOnboardingJobResponse'

instance Prelude.Hashable DeleteInstanceOnboardingJob where
  hashWithSalt _salt DeleteInstanceOnboardingJob' {..} =
    _salt `Prelude.hashWithSalt` connectInstanceId

instance Prelude.NFData DeleteInstanceOnboardingJob where
  rnf DeleteInstanceOnboardingJob' {..} =
    Prelude.rnf connectInstanceId

instance Data.ToHeaders DeleteInstanceOnboardingJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteInstanceOnboardingJob where
  toPath DeleteInstanceOnboardingJob' {..} =
    Prelude.mconcat
      [ "/connect-instance/",
        Data.toBS connectInstanceId,
        "/onboarding"
      ]

instance Data.ToQuery DeleteInstanceOnboardingJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteInstanceOnboardingJobResponse' smart constructor.
data DeleteInstanceOnboardingJobResponse = DeleteInstanceOnboardingJobResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteInstanceOnboardingJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteInstanceOnboardingJobResponse ::
  DeleteInstanceOnboardingJobResponse
newDeleteInstanceOnboardingJobResponse =
  DeleteInstanceOnboardingJobResponse'

instance
  Prelude.NFData
    DeleteInstanceOnboardingJobResponse
  where
  rnf _ = ()
