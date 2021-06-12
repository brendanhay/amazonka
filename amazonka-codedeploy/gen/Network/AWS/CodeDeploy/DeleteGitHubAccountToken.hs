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
-- Module      : Network.AWS.CodeDeploy.DeleteGitHubAccountToken
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a GitHub account connection.
module Network.AWS.CodeDeploy.DeleteGitHubAccountToken
  ( -- * Creating a Request
    DeleteGitHubAccountToken (..),
    newDeleteGitHubAccountToken,

    -- * Request Lenses
    deleteGitHubAccountToken_tokenName,

    -- * Destructuring the Response
    DeleteGitHubAccountTokenResponse (..),
    newDeleteGitHubAccountTokenResponse,

    -- * Response Lenses
    deleteGitHubAccountTokenResponse_tokenName,
    deleteGitHubAccountTokenResponse_httpStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DeleteGitHubAccount@ operation.
--
-- /See:/ 'newDeleteGitHubAccountToken' smart constructor.
data DeleteGitHubAccountToken = DeleteGitHubAccountToken'
  { -- | The name of the GitHub account connection to delete.
    tokenName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteGitHubAccountToken' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tokenName', 'deleteGitHubAccountToken_tokenName' - The name of the GitHub account connection to delete.
newDeleteGitHubAccountToken ::
  DeleteGitHubAccountToken
newDeleteGitHubAccountToken =
  DeleteGitHubAccountToken' {tokenName = Core.Nothing}

-- | The name of the GitHub account connection to delete.
deleteGitHubAccountToken_tokenName :: Lens.Lens' DeleteGitHubAccountToken (Core.Maybe Core.Text)
deleteGitHubAccountToken_tokenName = Lens.lens (\DeleteGitHubAccountToken' {tokenName} -> tokenName) (\s@DeleteGitHubAccountToken' {} a -> s {tokenName = a} :: DeleteGitHubAccountToken)

instance Core.AWSRequest DeleteGitHubAccountToken where
  type
    AWSResponse DeleteGitHubAccountToken =
      DeleteGitHubAccountTokenResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteGitHubAccountTokenResponse'
            Core.<$> (x Core..?> "tokenName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteGitHubAccountToken

instance Core.NFData DeleteGitHubAccountToken

instance Core.ToHeaders DeleteGitHubAccountToken where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeDeploy_20141006.DeleteGitHubAccountToken" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteGitHubAccountToken where
  toJSON DeleteGitHubAccountToken' {..} =
    Core.object
      ( Core.catMaybes
          [("tokenName" Core..=) Core.<$> tokenName]
      )

instance Core.ToPath DeleteGitHubAccountToken where
  toPath = Core.const "/"

instance Core.ToQuery DeleteGitHubAccountToken where
  toQuery = Core.const Core.mempty

-- | Represents the output of a @DeleteGitHubAccountToken@ operation.
--
-- /See:/ 'newDeleteGitHubAccountTokenResponse' smart constructor.
data DeleteGitHubAccountTokenResponse = DeleteGitHubAccountTokenResponse'
  { -- | The name of the GitHub account connection that was deleted.
    tokenName :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteGitHubAccountTokenResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tokenName', 'deleteGitHubAccountTokenResponse_tokenName' - The name of the GitHub account connection that was deleted.
--
-- 'httpStatus', 'deleteGitHubAccountTokenResponse_httpStatus' - The response's http status code.
newDeleteGitHubAccountTokenResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteGitHubAccountTokenResponse
newDeleteGitHubAccountTokenResponse pHttpStatus_ =
  DeleteGitHubAccountTokenResponse'
    { tokenName =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the GitHub account connection that was deleted.
deleteGitHubAccountTokenResponse_tokenName :: Lens.Lens' DeleteGitHubAccountTokenResponse (Core.Maybe Core.Text)
deleteGitHubAccountTokenResponse_tokenName = Lens.lens (\DeleteGitHubAccountTokenResponse' {tokenName} -> tokenName) (\s@DeleteGitHubAccountTokenResponse' {} a -> s {tokenName = a} :: DeleteGitHubAccountTokenResponse)

-- | The response's http status code.
deleteGitHubAccountTokenResponse_httpStatus :: Lens.Lens' DeleteGitHubAccountTokenResponse Core.Int
deleteGitHubAccountTokenResponse_httpStatus = Lens.lens (\DeleteGitHubAccountTokenResponse' {httpStatus} -> httpStatus) (\s@DeleteGitHubAccountTokenResponse' {} a -> s {httpStatus = a} :: DeleteGitHubAccountTokenResponse)

instance Core.NFData DeleteGitHubAccountTokenResponse
