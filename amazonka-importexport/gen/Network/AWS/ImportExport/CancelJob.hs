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
-- Module      : Network.AWS.ImportExport.CancelJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation cancels a specified job. Only the job owner can cancel
-- it. The operation fails if the job has already started or is complete.
module Network.AWS.ImportExport.CancelJob
  ( -- * Creating a Request
    CancelJob (..),
    newCancelJob,

    -- * Request Lenses
    cancelJob_aPIVersion,
    cancelJob_jobId,

    -- * Destructuring the Response
    CancelJobResponse (..),
    newCancelJobResponse,

    -- * Response Lenses
    cancelJobResponse_success,
    cancelJobResponse_httpStatus,
  )
where

import Network.AWS.ImportExport.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Input structure for the CancelJob operation.
--
-- /See:/ 'newCancelJob' smart constructor.
data CancelJob = CancelJob'
  { aPIVersion :: Prelude.Maybe Prelude.Text,
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CancelJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aPIVersion', 'cancelJob_aPIVersion' - Undocumented member.
--
-- 'jobId', 'cancelJob_jobId' - Undocumented member.
newCancelJob ::
  -- | 'jobId'
  Prelude.Text ->
  CancelJob
newCancelJob pJobId_ =
  CancelJob'
    { aPIVersion = Prelude.Nothing,
      jobId = pJobId_
    }

-- | Undocumented member.
cancelJob_aPIVersion :: Lens.Lens' CancelJob (Prelude.Maybe Prelude.Text)
cancelJob_aPIVersion = Lens.lens (\CancelJob' {aPIVersion} -> aPIVersion) (\s@CancelJob' {} a -> s {aPIVersion = a} :: CancelJob)

-- | Undocumented member.
cancelJob_jobId :: Lens.Lens' CancelJob Prelude.Text
cancelJob_jobId = Lens.lens (\CancelJob' {jobId} -> jobId) (\s@CancelJob' {} a -> s {jobId = a} :: CancelJob)

instance Prelude.AWSRequest CancelJob where
  type Rs CancelJob = CancelJobResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CancelJobResult"
      ( \s h x ->
          CancelJobResponse'
            Prelude.<$> (x Prelude..@? "Success")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelJob

instance Prelude.NFData CancelJob

instance Prelude.ToHeaders CancelJob where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath CancelJob where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CancelJob where
  toQuery CancelJob' {..} =
    Prelude.mconcat
      [ "Operation=CancelJob",
        "Action"
          Prelude.=: ("CancelJob" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-06-01" :: Prelude.ByteString),
        "APIVersion" Prelude.=: aPIVersion,
        "JobId" Prelude.=: jobId
      ]

-- | Output structure for the CancelJob operation.
--
-- /See:/ 'newCancelJobResponse' smart constructor.
data CancelJobResponse = CancelJobResponse'
  { success :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CancelJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'success', 'cancelJobResponse_success' - Undocumented member.
--
-- 'httpStatus', 'cancelJobResponse_httpStatus' - The response's http status code.
newCancelJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelJobResponse
newCancelJobResponse pHttpStatus_ =
  CancelJobResponse'
    { success = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
cancelJobResponse_success :: Lens.Lens' CancelJobResponse (Prelude.Maybe Prelude.Bool)
cancelJobResponse_success = Lens.lens (\CancelJobResponse' {success} -> success) (\s@CancelJobResponse' {} a -> s {success = a} :: CancelJobResponse)

-- | The response's http status code.
cancelJobResponse_httpStatus :: Lens.Lens' CancelJobResponse Prelude.Int
cancelJobResponse_httpStatus = Lens.lens (\CancelJobResponse' {httpStatus} -> httpStatus) (\s@CancelJobResponse' {} a -> s {httpStatus = a} :: CancelJobResponse)

instance Prelude.NFData CancelJobResponse
