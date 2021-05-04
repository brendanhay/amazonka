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
-- Module      : Network.AWS.CodeCommit.UpdatePullRequestDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces the contents of the description of a pull request.
module Network.AWS.CodeCommit.UpdatePullRequestDescription
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

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdatePullRequestDescription' smart constructor.
data UpdatePullRequestDescription = UpdatePullRequestDescription'
  { -- | The system-generated ID of the pull request. To get this ID, use
    -- ListPullRequests.
    pullRequestId :: Prelude.Text,
    -- | The updated content of the description for the pull request. This
    -- content replaces the existing description.
    description :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance
  Prelude.AWSRequest
    UpdatePullRequestDescription
  where
  type
    Rs UpdatePullRequestDescription =
      UpdatePullRequestDescriptionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdatePullRequestDescriptionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "pullRequest")
      )

instance
  Prelude.Hashable
    UpdatePullRequestDescription

instance Prelude.NFData UpdatePullRequestDescription

instance
  Prelude.ToHeaders
    UpdatePullRequestDescription
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeCommit_20150413.UpdatePullRequestDescription" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdatePullRequestDescription where
  toJSON UpdatePullRequestDescription' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("pullRequestId" Prelude..= pullRequestId),
            Prelude.Just ("description" Prelude..= description)
          ]
      )

instance Prelude.ToPath UpdatePullRequestDescription where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdatePullRequestDescription where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdatePullRequestDescriptionResponse' smart constructor.
data UpdatePullRequestDescriptionResponse = UpdatePullRequestDescriptionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about the updated pull request.
    pullRequest :: PullRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
