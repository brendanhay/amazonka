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
-- Module      : Network.AWS.CodeBuild.DeleteSourceCredentials
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a set of GitHub, GitHub Enterprise, or Bitbucket source
-- credentials.
module Network.AWS.CodeBuild.DeleteSourceCredentials
  ( -- * Creating a Request
    DeleteSourceCredentials (..),
    newDeleteSourceCredentials,

    -- * Request Lenses
    deleteSourceCredentials_arn,

    -- * Destructuring the Response
    DeleteSourceCredentialsResponse (..),
    newDeleteSourceCredentialsResponse,

    -- * Response Lenses
    deleteSourceCredentialsResponse_arn,
    deleteSourceCredentialsResponse_httpStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteSourceCredentials' smart constructor.
data DeleteSourceCredentials = DeleteSourceCredentials'
  { -- | The Amazon Resource Name (ARN) of the token.
    arn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteSourceCredentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deleteSourceCredentials_arn' - The Amazon Resource Name (ARN) of the token.
newDeleteSourceCredentials ::
  -- | 'arn'
  Core.Text ->
  DeleteSourceCredentials
newDeleteSourceCredentials pArn_ =
  DeleteSourceCredentials' {arn = pArn_}

-- | The Amazon Resource Name (ARN) of the token.
deleteSourceCredentials_arn :: Lens.Lens' DeleteSourceCredentials Core.Text
deleteSourceCredentials_arn = Lens.lens (\DeleteSourceCredentials' {arn} -> arn) (\s@DeleteSourceCredentials' {} a -> s {arn = a} :: DeleteSourceCredentials)

instance Core.AWSRequest DeleteSourceCredentials where
  type
    AWSResponse DeleteSourceCredentials =
      DeleteSourceCredentialsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteSourceCredentialsResponse'
            Core.<$> (x Core..?> "arn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteSourceCredentials

instance Core.NFData DeleteSourceCredentials

instance Core.ToHeaders DeleteSourceCredentials where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeBuild_20161006.DeleteSourceCredentials" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteSourceCredentials where
  toJSON DeleteSourceCredentials' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("arn" Core..= arn)])

instance Core.ToPath DeleteSourceCredentials where
  toPath = Core.const "/"

instance Core.ToQuery DeleteSourceCredentials where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteSourceCredentialsResponse' smart constructor.
data DeleteSourceCredentialsResponse = DeleteSourceCredentialsResponse'
  { -- | The Amazon Resource Name (ARN) of the token.
    arn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteSourceCredentialsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deleteSourceCredentialsResponse_arn' - The Amazon Resource Name (ARN) of the token.
--
-- 'httpStatus', 'deleteSourceCredentialsResponse_httpStatus' - The response's http status code.
newDeleteSourceCredentialsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteSourceCredentialsResponse
newDeleteSourceCredentialsResponse pHttpStatus_ =
  DeleteSourceCredentialsResponse'
    { arn =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the token.
deleteSourceCredentialsResponse_arn :: Lens.Lens' DeleteSourceCredentialsResponse (Core.Maybe Core.Text)
deleteSourceCredentialsResponse_arn = Lens.lens (\DeleteSourceCredentialsResponse' {arn} -> arn) (\s@DeleteSourceCredentialsResponse' {} a -> s {arn = a} :: DeleteSourceCredentialsResponse)

-- | The response's http status code.
deleteSourceCredentialsResponse_httpStatus :: Lens.Lens' DeleteSourceCredentialsResponse Core.Int
deleteSourceCredentialsResponse_httpStatus = Lens.lens (\DeleteSourceCredentialsResponse' {httpStatus} -> httpStatus) (\s@DeleteSourceCredentialsResponse' {} a -> s {httpStatus = a} :: DeleteSourceCredentialsResponse)

instance Core.NFData DeleteSourceCredentialsResponse
