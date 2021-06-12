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
-- Module      : Network.AWS.CognitoIdentityProvider.DeleteUserPoolDomain
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a domain for a user pool.
module Network.AWS.CognitoIdentityProvider.DeleteUserPoolDomain
  ( -- * Creating a Request
    DeleteUserPoolDomain (..),
    newDeleteUserPoolDomain,

    -- * Request Lenses
    deleteUserPoolDomain_domain,
    deleteUserPoolDomain_userPoolId,

    -- * Destructuring the Response
    DeleteUserPoolDomainResponse (..),
    newDeleteUserPoolDomainResponse,

    -- * Response Lenses
    deleteUserPoolDomainResponse_httpStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteUserPoolDomain' smart constructor.
data DeleteUserPoolDomain = DeleteUserPoolDomain'
  { -- | The domain string.
    domain :: Core.Text,
    -- | The user pool ID.
    userPoolId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteUserPoolDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domain', 'deleteUserPoolDomain_domain' - The domain string.
--
-- 'userPoolId', 'deleteUserPoolDomain_userPoolId' - The user pool ID.
newDeleteUserPoolDomain ::
  -- | 'domain'
  Core.Text ->
  -- | 'userPoolId'
  Core.Text ->
  DeleteUserPoolDomain
newDeleteUserPoolDomain pDomain_ pUserPoolId_ =
  DeleteUserPoolDomain'
    { domain = pDomain_,
      userPoolId = pUserPoolId_
    }

-- | The domain string.
deleteUserPoolDomain_domain :: Lens.Lens' DeleteUserPoolDomain Core.Text
deleteUserPoolDomain_domain = Lens.lens (\DeleteUserPoolDomain' {domain} -> domain) (\s@DeleteUserPoolDomain' {} a -> s {domain = a} :: DeleteUserPoolDomain)

-- | The user pool ID.
deleteUserPoolDomain_userPoolId :: Lens.Lens' DeleteUserPoolDomain Core.Text
deleteUserPoolDomain_userPoolId = Lens.lens (\DeleteUserPoolDomain' {userPoolId} -> userPoolId) (\s@DeleteUserPoolDomain' {} a -> s {userPoolId = a} :: DeleteUserPoolDomain)

instance Core.AWSRequest DeleteUserPoolDomain where
  type
    AWSResponse DeleteUserPoolDomain =
      DeleteUserPoolDomainResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteUserPoolDomainResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteUserPoolDomain

instance Core.NFData DeleteUserPoolDomain

instance Core.ToHeaders DeleteUserPoolDomain where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.DeleteUserPoolDomain" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteUserPoolDomain where
  toJSON DeleteUserPoolDomain' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Domain" Core..= domain),
            Core.Just ("UserPoolId" Core..= userPoolId)
          ]
      )

instance Core.ToPath DeleteUserPoolDomain where
  toPath = Core.const "/"

instance Core.ToQuery DeleteUserPoolDomain where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteUserPoolDomainResponse' smart constructor.
data DeleteUserPoolDomainResponse = DeleteUserPoolDomainResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteUserPoolDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteUserPoolDomainResponse_httpStatus' - The response's http status code.
newDeleteUserPoolDomainResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteUserPoolDomainResponse
newDeleteUserPoolDomainResponse pHttpStatus_ =
  DeleteUserPoolDomainResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteUserPoolDomainResponse_httpStatus :: Lens.Lens' DeleteUserPoolDomainResponse Core.Int
deleteUserPoolDomainResponse_httpStatus = Lens.lens (\DeleteUserPoolDomainResponse' {httpStatus} -> httpStatus) (\s@DeleteUserPoolDomainResponse' {} a -> s {httpStatus = a} :: DeleteUserPoolDomainResponse)

instance Core.NFData DeleteUserPoolDomainResponse
