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
-- Module      : Amazonka.CodeCommit.UpdatePullRequestDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces the contents of the description of a pull request.
module Amazonka.CodeCommit.UpdatePullRequestDescription
  ( -- * Creating a Request
    UpdatePullRequestDescription (..),
    newUpdatePullRequestDescription,

    -- * Request Lenses
    updatePullRequestDescription_pullRequestId,
    updatePullRequestDescription_description,

    -- * Destructuring the Response
    UpdatePullRequestDescriptionResponse (..),
    newUpdatePullRequestDescriptionResponse,

    -- * Response Lenses
    updatePullRequestDescriptionResponse_httpStatus,
    updatePullRequestDescriptionResponse_pullRequest,
  )
where

import Amazonka.CodeCommit.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdatePullRequestDescription' smart constructor.
data UpdatePullRequestDescription = UpdatePullRequestDescription'
  { -- | The system-generated ID of the pull request. To get this ID, use
    -- ListPullRequests.
    pullRequestId :: Prelude.Text,
    -- | The updated content of the description for the pull request. This
    -- content replaces the existing description.
    description :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePullRequestDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pullRequestId', 'updatePullRequestDescription_pullRequestId' - The system-generated ID of the pull request. To get this ID, use
-- ListPullRequests.
--
-- 'description', 'updatePullRequestDescription_description' - The updated content of the description for the pull request. This
-- content replaces the existing description.
newUpdatePullRequestDescription ::
  -- | 'pullRequestId'
  Prelude.Text ->
  -- | 'description'
  Prelude.Text ->
  UpdatePullRequestDescription
newUpdatePullRequestDescription
  pPullRequestId_
  pDescription_ =
    UpdatePullRequestDescription'
      { pullRequestId =
          pPullRequestId_,
        description = pDescription_
      }

-- | The system-generated ID of the pull request. To get this ID, use
-- ListPullRequests.
updatePullRequestDescription_pullRequestId :: Lens.Lens' UpdatePullRequestDescription Prelude.Text
updatePullRequestDescription_pullRequestId = Lens.lens (\UpdatePullRequestDescription' {pullRequestId} -> pullRequestId) (\s@UpdatePullRequestDescription' {} a -> s {pullRequestId = a} :: UpdatePullRequestDescription)

-- | The updated content of the description for the pull request. This
-- content replaces the existing description.
updatePullRequestDescription_description :: Lens.Lens' UpdatePullRequestDescription Prelude.Text
updatePullRequestDescription_description = Lens.lens (\UpdatePullRequestDescription' {description} -> description) (\s@UpdatePullRequestDescription' {} a -> s {description = a} :: UpdatePullRequestDescription)

instance Core.AWSRequest UpdatePullRequestDescription where
  type
    AWSResponse UpdatePullRequestDescription =
      UpdatePullRequestDescriptionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdatePullRequestDescriptionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "pullRequest")
      )

instance
  Prelude.Hashable
    UpdatePullRequestDescription
  where
  hashWithSalt _salt UpdatePullRequestDescription' {..} =
    _salt
      `Prelude.hashWithSalt` pullRequestId
      `Prelude.hashWithSalt` description

instance Prelude.NFData UpdatePullRequestDescription where
  rnf UpdatePullRequestDescription' {..} =
    Prelude.rnf pullRequestId
      `Prelude.seq` Prelude.rnf description

instance Data.ToHeaders UpdatePullRequestDescription where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeCommit_20150413.UpdatePullRequestDescription" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdatePullRequestDescription where
  toJSON UpdatePullRequestDescription' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("pullRequestId" Data..= pullRequestId),
            Prelude.Just ("description" Data..= description)
          ]
      )

instance Data.ToPath UpdatePullRequestDescription where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdatePullRequestDescription where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdatePullRequestDescriptionResponse' smart constructor.
data UpdatePullRequestDescriptionResponse = UpdatePullRequestDescriptionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about the updated pull request.
    pullRequest :: PullRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePullRequestDescriptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updatePullRequestDescriptionResponse_httpStatus' - The response's http status code.
--
-- 'pullRequest', 'updatePullRequestDescriptionResponse_pullRequest' - Information about the updated pull request.
newUpdatePullRequestDescriptionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'pullRequest'
  PullRequest ->
  UpdatePullRequestDescriptionResponse
newUpdatePullRequestDescriptionResponse
  pHttpStatus_
  pPullRequest_ =
    UpdatePullRequestDescriptionResponse'
      { httpStatus =
          pHttpStatus_,
        pullRequest = pPullRequest_
      }

-- | The response's http status code.
updatePullRequestDescriptionResponse_httpStatus :: Lens.Lens' UpdatePullRequestDescriptionResponse Prelude.Int
updatePullRequestDescriptionResponse_httpStatus = Lens.lens (\UpdatePullRequestDescriptionResponse' {httpStatus} -> httpStatus) (\s@UpdatePullRequestDescriptionResponse' {} a -> s {httpStatus = a} :: UpdatePullRequestDescriptionResponse)

-- | Information about the updated pull request.
updatePullRequestDescriptionResponse_pullRequest :: Lens.Lens' UpdatePullRequestDescriptionResponse PullRequest
updatePullRequestDescriptionResponse_pullRequest = Lens.lens (\UpdatePullRequestDescriptionResponse' {pullRequest} -> pullRequest) (\s@UpdatePullRequestDescriptionResponse' {} a -> s {pullRequest = a} :: UpdatePullRequestDescriptionResponse)

instance
  Prelude.NFData
    UpdatePullRequestDescriptionResponse
  where
  rnf UpdatePullRequestDescriptionResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf pullRequest
