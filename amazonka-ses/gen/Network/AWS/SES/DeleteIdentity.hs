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
-- Module      : Network.AWS.SES.DeleteIdentity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified identity (an email address or a domain) from the
-- list of verified identities.
--
-- You can execute this operation no more than once per second.
module Network.AWS.SES.DeleteIdentity
  ( -- * Creating a Request
    DeleteIdentity (..),
    newDeleteIdentity,

    -- * Request Lenses
    deleteIdentity_identity,

    -- * Destructuring the Response
    DeleteIdentityResponse (..),
    newDeleteIdentityResponse,

    -- * Response Lenses
    deleteIdentityResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SES.Types

-- | Represents a request to delete one of your Amazon SES identities (an
-- email address or domain).
--
-- /See:/ 'newDeleteIdentity' smart constructor.
data DeleteIdentity = DeleteIdentity'
  { -- | The identity to be removed from the list of identities for the AWS
    -- Account.
    identity :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteIdentity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identity', 'deleteIdentity_identity' - The identity to be removed from the list of identities for the AWS
-- Account.
newDeleteIdentity ::
  -- | 'identity'
  Core.Text ->
  DeleteIdentity
newDeleteIdentity pIdentity_ =
  DeleteIdentity' {identity = pIdentity_}

-- | The identity to be removed from the list of identities for the AWS
-- Account.
deleteIdentity_identity :: Lens.Lens' DeleteIdentity Core.Text
deleteIdentity_identity = Lens.lens (\DeleteIdentity' {identity} -> identity) (\s@DeleteIdentity' {} a -> s {identity = a} :: DeleteIdentity)

instance Core.AWSRequest DeleteIdentity where
  type
    AWSResponse DeleteIdentity =
      DeleteIdentityResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DeleteIdentityResult"
      ( \s h x ->
          DeleteIdentityResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteIdentity

instance Core.NFData DeleteIdentity

instance Core.ToHeaders DeleteIdentity where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteIdentity where
  toPath = Core.const "/"

instance Core.ToQuery DeleteIdentity where
  toQuery DeleteIdentity' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DeleteIdentity" :: Core.ByteString),
        "Version" Core.=: ("2010-12-01" :: Core.ByteString),
        "Identity" Core.=: identity
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'newDeleteIdentityResponse' smart constructor.
data DeleteIdentityResponse = DeleteIdentityResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteIdentityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteIdentityResponse_httpStatus' - The response's http status code.
newDeleteIdentityResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteIdentityResponse
newDeleteIdentityResponse pHttpStatus_ =
  DeleteIdentityResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteIdentityResponse_httpStatus :: Lens.Lens' DeleteIdentityResponse Core.Int
deleteIdentityResponse_httpStatus = Lens.lens (\DeleteIdentityResponse' {httpStatus} -> httpStatus) (\s@DeleteIdentityResponse' {} a -> s {httpStatus = a} :: DeleteIdentityResponse)

instance Core.NFData DeleteIdentityResponse
