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
-- Module      : Network.AWS.CodeDeploy.BatchGetApplications
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about one or more applications. The maximum number of
-- applications that can be returned is 100.
module Network.AWS.CodeDeploy.BatchGetApplications
  ( -- * Creating a Request
    BatchGetApplications (..),
    newBatchGetApplications,

    -- * Request Lenses
    batchGetApplications_applicationNames,

    -- * Destructuring the Response
    BatchGetApplicationsResponse (..),
    newBatchGetApplicationsResponse,

    -- * Response Lenses
    batchGetApplicationsResponse_applicationsInfo,
    batchGetApplicationsResponse_httpStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @BatchGetApplications@ operation.
--
-- /See:/ 'newBatchGetApplications' smart constructor.
data BatchGetApplications = BatchGetApplications'
  { -- | A list of application names separated by spaces. The maximum number of
    -- application names you can specify is 100.
    applicationNames :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetApplications' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationNames', 'batchGetApplications_applicationNames' - A list of application names separated by spaces. The maximum number of
-- application names you can specify is 100.
newBatchGetApplications ::
  BatchGetApplications
newBatchGetApplications =
  BatchGetApplications'
    { applicationNames =
        Prelude.mempty
    }

-- | A list of application names separated by spaces. The maximum number of
-- application names you can specify is 100.
batchGetApplications_applicationNames :: Lens.Lens' BatchGetApplications [Prelude.Text]
batchGetApplications_applicationNames = Lens.lens (\BatchGetApplications' {applicationNames} -> applicationNames) (\s@BatchGetApplications' {} a -> s {applicationNames = a} :: BatchGetApplications) Prelude.. Lens._Coerce

instance Core.AWSRequest BatchGetApplications where
  type
    AWSResponse BatchGetApplications =
      BatchGetApplicationsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetApplicationsResponse'
            Prelude.<$> ( x Core..?> "applicationsInfo"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchGetApplications

instance Prelude.NFData BatchGetApplications

instance Core.ToHeaders BatchGetApplications where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeDeploy_20141006.BatchGetApplications" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON BatchGetApplications where
  toJSON BatchGetApplications' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("applicationNames" Core..= applicationNames)
          ]
      )

instance Core.ToPath BatchGetApplications where
  toPath = Prelude.const "/"

instance Core.ToQuery BatchGetApplications where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @BatchGetApplications@ operation.
--
-- /See:/ 'newBatchGetApplicationsResponse' smart constructor.
data BatchGetApplicationsResponse = BatchGetApplicationsResponse'
  { -- | Information about the applications.
    applicationsInfo :: Prelude.Maybe [ApplicationInfo],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetApplicationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationsInfo', 'batchGetApplicationsResponse_applicationsInfo' - Information about the applications.
--
-- 'httpStatus', 'batchGetApplicationsResponse_httpStatus' - The response's http status code.
newBatchGetApplicationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchGetApplicationsResponse
newBatchGetApplicationsResponse pHttpStatus_ =
  BatchGetApplicationsResponse'
    { applicationsInfo =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the applications.
batchGetApplicationsResponse_applicationsInfo :: Lens.Lens' BatchGetApplicationsResponse (Prelude.Maybe [ApplicationInfo])
batchGetApplicationsResponse_applicationsInfo = Lens.lens (\BatchGetApplicationsResponse' {applicationsInfo} -> applicationsInfo) (\s@BatchGetApplicationsResponse' {} a -> s {applicationsInfo = a} :: BatchGetApplicationsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
batchGetApplicationsResponse_httpStatus :: Lens.Lens' BatchGetApplicationsResponse Prelude.Int
batchGetApplicationsResponse_httpStatus = Lens.lens (\BatchGetApplicationsResponse' {httpStatus} -> httpStatus) (\s@BatchGetApplicationsResponse' {} a -> s {httpStatus = a} :: BatchGetApplicationsResponse)

instance Prelude.NFData BatchGetApplicationsResponse
