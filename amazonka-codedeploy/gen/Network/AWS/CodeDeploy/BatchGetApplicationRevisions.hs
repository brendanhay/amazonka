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
-- Module      : Network.AWS.CodeDeploy.BatchGetApplicationRevisions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about one or more application revisions. The maximum
-- number of application revisions that can be returned is 25.
module Network.AWS.CodeDeploy.BatchGetApplicationRevisions
  ( -- * Creating a Request
    BatchGetApplicationRevisions (..),
    newBatchGetApplicationRevisions,

    -- * Request Lenses
    batchGetApplicationRevisions_applicationName,
    batchGetApplicationRevisions_revisions,

    -- * Destructuring the Response
    BatchGetApplicationRevisionsResponse (..),
    newBatchGetApplicationRevisionsResponse,

    -- * Response Lenses
    batchGetApplicationRevisionsResponse_revisions,
    batchGetApplicationRevisionsResponse_errorMessage,
    batchGetApplicationRevisionsResponse_applicationName,
    batchGetApplicationRevisionsResponse_httpStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @BatchGetApplicationRevisions@ operation.
--
-- /See:/ 'newBatchGetApplicationRevisions' smart constructor.
data BatchGetApplicationRevisions = BatchGetApplicationRevisions'
  { -- | The name of an AWS CodeDeploy application about which to get revision
    -- information.
    applicationName :: Prelude.Text,
    -- | An array of @RevisionLocation@ objects that specify information to get
    -- about the application revisions, including type and location. The
    -- maximum number of @RevisionLocation@ objects you can specify is 25.
    revisions :: [RevisionLocation]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetApplicationRevisions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationName', 'batchGetApplicationRevisions_applicationName' - The name of an AWS CodeDeploy application about which to get revision
-- information.
--
-- 'revisions', 'batchGetApplicationRevisions_revisions' - An array of @RevisionLocation@ objects that specify information to get
-- about the application revisions, including type and location. The
-- maximum number of @RevisionLocation@ objects you can specify is 25.
newBatchGetApplicationRevisions ::
  -- | 'applicationName'
  Prelude.Text ->
  BatchGetApplicationRevisions
newBatchGetApplicationRevisions pApplicationName_ =
  BatchGetApplicationRevisions'
    { applicationName =
        pApplicationName_,
      revisions = Prelude.mempty
    }

-- | The name of an AWS CodeDeploy application about which to get revision
-- information.
batchGetApplicationRevisions_applicationName :: Lens.Lens' BatchGetApplicationRevisions Prelude.Text
batchGetApplicationRevisions_applicationName = Lens.lens (\BatchGetApplicationRevisions' {applicationName} -> applicationName) (\s@BatchGetApplicationRevisions' {} a -> s {applicationName = a} :: BatchGetApplicationRevisions)

-- | An array of @RevisionLocation@ objects that specify information to get
-- about the application revisions, including type and location. The
-- maximum number of @RevisionLocation@ objects you can specify is 25.
batchGetApplicationRevisions_revisions :: Lens.Lens' BatchGetApplicationRevisions [RevisionLocation]
batchGetApplicationRevisions_revisions = Lens.lens (\BatchGetApplicationRevisions' {revisions} -> revisions) (\s@BatchGetApplicationRevisions' {} a -> s {revisions = a} :: BatchGetApplicationRevisions) Prelude.. Lens._Coerce

instance Core.AWSRequest BatchGetApplicationRevisions where
  type
    AWSResponse BatchGetApplicationRevisions =
      BatchGetApplicationRevisionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetApplicationRevisionsResponse'
            Prelude.<$> (x Core..?> "revisions" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "errorMessage")
            Prelude.<*> (x Core..?> "applicationName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    BatchGetApplicationRevisions

instance Prelude.NFData BatchGetApplicationRevisions

instance Core.ToHeaders BatchGetApplicationRevisions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeDeploy_20141006.BatchGetApplicationRevisions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON BatchGetApplicationRevisions where
  toJSON BatchGetApplicationRevisions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("applicationName" Core..= applicationName),
            Prelude.Just ("revisions" Core..= revisions)
          ]
      )

instance Core.ToPath BatchGetApplicationRevisions where
  toPath = Prelude.const "/"

instance Core.ToQuery BatchGetApplicationRevisions where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @BatchGetApplicationRevisions@ operation.
--
-- /See:/ 'newBatchGetApplicationRevisionsResponse' smart constructor.
data BatchGetApplicationRevisionsResponse = BatchGetApplicationRevisionsResponse'
  { -- | Additional information about the revisions, including the type and
    -- location.
    revisions :: Prelude.Maybe [RevisionInfo],
    -- | Information about errors that might have occurred during the API call.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The name of the application that corresponds to the revisions.
    applicationName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetApplicationRevisionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'revisions', 'batchGetApplicationRevisionsResponse_revisions' - Additional information about the revisions, including the type and
-- location.
--
-- 'errorMessage', 'batchGetApplicationRevisionsResponse_errorMessage' - Information about errors that might have occurred during the API call.
--
-- 'applicationName', 'batchGetApplicationRevisionsResponse_applicationName' - The name of the application that corresponds to the revisions.
--
-- 'httpStatus', 'batchGetApplicationRevisionsResponse_httpStatus' - The response's http status code.
newBatchGetApplicationRevisionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchGetApplicationRevisionsResponse
newBatchGetApplicationRevisionsResponse pHttpStatus_ =
  BatchGetApplicationRevisionsResponse'
    { revisions =
        Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      applicationName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Additional information about the revisions, including the type and
-- location.
batchGetApplicationRevisionsResponse_revisions :: Lens.Lens' BatchGetApplicationRevisionsResponse (Prelude.Maybe [RevisionInfo])
batchGetApplicationRevisionsResponse_revisions = Lens.lens (\BatchGetApplicationRevisionsResponse' {revisions} -> revisions) (\s@BatchGetApplicationRevisionsResponse' {} a -> s {revisions = a} :: BatchGetApplicationRevisionsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | Information about errors that might have occurred during the API call.
batchGetApplicationRevisionsResponse_errorMessage :: Lens.Lens' BatchGetApplicationRevisionsResponse (Prelude.Maybe Prelude.Text)
batchGetApplicationRevisionsResponse_errorMessage = Lens.lens (\BatchGetApplicationRevisionsResponse' {errorMessage} -> errorMessage) (\s@BatchGetApplicationRevisionsResponse' {} a -> s {errorMessage = a} :: BatchGetApplicationRevisionsResponse)

-- | The name of the application that corresponds to the revisions.
batchGetApplicationRevisionsResponse_applicationName :: Lens.Lens' BatchGetApplicationRevisionsResponse (Prelude.Maybe Prelude.Text)
batchGetApplicationRevisionsResponse_applicationName = Lens.lens (\BatchGetApplicationRevisionsResponse' {applicationName} -> applicationName) (\s@BatchGetApplicationRevisionsResponse' {} a -> s {applicationName = a} :: BatchGetApplicationRevisionsResponse)

-- | The response's http status code.
batchGetApplicationRevisionsResponse_httpStatus :: Lens.Lens' BatchGetApplicationRevisionsResponse Prelude.Int
batchGetApplicationRevisionsResponse_httpStatus = Lens.lens (\BatchGetApplicationRevisionsResponse' {httpStatus} -> httpStatus) (\s@BatchGetApplicationRevisionsResponse' {} a -> s {httpStatus = a} :: BatchGetApplicationRevisionsResponse)

instance
  Prelude.NFData
    BatchGetApplicationRevisionsResponse
