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
-- Module      : Network.AWS.CodeCommit.CreatePullRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a pull request in the specified repository.
module Network.AWS.CodeCommit.CreatePullRequest
  ( -- * Creating a Request
    CreatePullRequest (..),
    newCreatePullRequest,

    -- * Request Lenses
    createPullRequest_description,
    createPullRequest_clientRequestToken,
    createPullRequest_title,
    createPullRequest_targets,

    -- * Destructuring the Response
    CreatePullRequestResponse (..),
    newCreatePullRequestResponse,

    -- * Response Lenses
    createPullRequestResponse_httpStatus,
    createPullRequestResponse_pullRequest,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreatePullRequest' smart constructor.
data CreatePullRequest = CreatePullRequest'
  { -- | A description of the pull request.
    description :: Prelude.Maybe Prelude.Text,
    -- | A unique, client-generated idempotency token that, when provided in a
    -- request, ensures the request cannot be repeated with a changed
    -- parameter. If a request is received with the same parameters and a token
    -- is included, the request returns information about the initial request
    -- that used that token.
    --
    -- The AWS SDKs prepopulate client request tokens. If you are using an AWS
    -- SDK, an idempotency token is created for you.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The title of the pull request. This title is used to identify the pull
    -- request to other users in the repository.
    title :: Prelude.Text,
    -- | The targets for the pull request, including the source of the code to be
    -- reviewed (the source branch) and the destination where the creator of
    -- the pull request intends the code to be merged after the pull request is
    -- closed (the destination branch).
    targets :: [Target]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreatePullRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createPullRequest_description' - A description of the pull request.
--
-- 'clientRequestToken', 'createPullRequest_clientRequestToken' - A unique, client-generated idempotency token that, when provided in a
-- request, ensures the request cannot be repeated with a changed
-- parameter. If a request is received with the same parameters and a token
-- is included, the request returns information about the initial request
-- that used that token.
--
-- The AWS SDKs prepopulate client request tokens. If you are using an AWS
-- SDK, an idempotency token is created for you.
--
-- 'title', 'createPullRequest_title' - The title of the pull request. This title is used to identify the pull
-- request to other users in the repository.
--
-- 'targets', 'createPullRequest_targets' - The targets for the pull request, including the source of the code to be
-- reviewed (the source branch) and the destination where the creator of
-- the pull request intends the code to be merged after the pull request is
-- closed (the destination branch).
newCreatePullRequest ::
  -- | 'title'
  Prelude.Text ->
  CreatePullRequest
newCreatePullRequest pTitle_ =
  CreatePullRequest'
    { description = Prelude.Nothing,
      clientRequestToken = Prelude.Nothing,
      title = pTitle_,
      targets = Prelude.mempty
    }

-- | A description of the pull request.
createPullRequest_description :: Lens.Lens' CreatePullRequest (Prelude.Maybe Prelude.Text)
createPullRequest_description = Lens.lens (\CreatePullRequest' {description} -> description) (\s@CreatePullRequest' {} a -> s {description = a} :: CreatePullRequest)

-- | A unique, client-generated idempotency token that, when provided in a
-- request, ensures the request cannot be repeated with a changed
-- parameter. If a request is received with the same parameters and a token
-- is included, the request returns information about the initial request
-- that used that token.
--
-- The AWS SDKs prepopulate client request tokens. If you are using an AWS
-- SDK, an idempotency token is created for you.
createPullRequest_clientRequestToken :: Lens.Lens' CreatePullRequest (Prelude.Maybe Prelude.Text)
createPullRequest_clientRequestToken = Lens.lens (\CreatePullRequest' {clientRequestToken} -> clientRequestToken) (\s@CreatePullRequest' {} a -> s {clientRequestToken = a} :: CreatePullRequest)

-- | The title of the pull request. This title is used to identify the pull
-- request to other users in the repository.
createPullRequest_title :: Lens.Lens' CreatePullRequest Prelude.Text
createPullRequest_title = Lens.lens (\CreatePullRequest' {title} -> title) (\s@CreatePullRequest' {} a -> s {title = a} :: CreatePullRequest)

-- | The targets for the pull request, including the source of the code to be
-- reviewed (the source branch) and the destination where the creator of
-- the pull request intends the code to be merged after the pull request is
-- closed (the destination branch).
createPullRequest_targets :: Lens.Lens' CreatePullRequest [Target]
createPullRequest_targets = Lens.lens (\CreatePullRequest' {targets} -> targets) (\s@CreatePullRequest' {} a -> s {targets = a} :: CreatePullRequest) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest CreatePullRequest where
  type Rs CreatePullRequest = CreatePullRequestResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePullRequestResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "pullRequest")
      )

instance Prelude.Hashable CreatePullRequest

instance Prelude.NFData CreatePullRequest

instance Prelude.ToHeaders CreatePullRequest where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeCommit_20150413.CreatePullRequest" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreatePullRequest where
  toJSON CreatePullRequest' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("description" Prelude..=) Prelude.<$> description,
            ("clientRequestToken" Prelude..=)
              Prelude.<$> clientRequestToken,
            Prelude.Just ("title" Prelude..= title),
            Prelude.Just ("targets" Prelude..= targets)
          ]
      )

instance Prelude.ToPath CreatePullRequest where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreatePullRequest where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreatePullRequestResponse' smart constructor.
data CreatePullRequestResponse = CreatePullRequestResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about the newly created pull request.
    pullRequest :: PullRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreatePullRequestResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createPullRequestResponse_httpStatus' - The response's http status code.
--
-- 'pullRequest', 'createPullRequestResponse_pullRequest' - Information about the newly created pull request.
newCreatePullRequestResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'pullRequest'
  PullRequest ->
  CreatePullRequestResponse
newCreatePullRequestResponse
  pHttpStatus_
  pPullRequest_ =
    CreatePullRequestResponse'
      { httpStatus =
          pHttpStatus_,
        pullRequest = pPullRequest_
      }

-- | The response's http status code.
createPullRequestResponse_httpStatus :: Lens.Lens' CreatePullRequestResponse Prelude.Int
createPullRequestResponse_httpStatus = Lens.lens (\CreatePullRequestResponse' {httpStatus} -> httpStatus) (\s@CreatePullRequestResponse' {} a -> s {httpStatus = a} :: CreatePullRequestResponse)

-- | Information about the newly created pull request.
createPullRequestResponse_pullRequest :: Lens.Lens' CreatePullRequestResponse PullRequest
createPullRequestResponse_pullRequest = Lens.lens (\CreatePullRequestResponse' {pullRequest} -> pullRequest) (\s@CreatePullRequestResponse' {} a -> s {pullRequest = a} :: CreatePullRequestResponse)

instance Prelude.NFData CreatePullRequestResponse
