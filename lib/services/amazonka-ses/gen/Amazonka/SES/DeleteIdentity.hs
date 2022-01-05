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
-- Module      : Amazonka.SES.DeleteIdentity
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
module Amazonka.SES.DeleteIdentity
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SES.Types

-- | Represents a request to delete one of your Amazon SES identities (an
-- email address or domain).
--
-- /See:/ 'newDeleteIdentity' smart constructor.
data DeleteIdentity = DeleteIdentity'
  { -- | The identity to be removed from the list of identities for the AWS
    -- Account.
    identity :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DeleteIdentity
newDeleteIdentity pIdentity_ =
  DeleteIdentity' {identity = pIdentity_}

-- | The identity to be removed from the list of identities for the AWS
-- Account.
deleteIdentity_identity :: Lens.Lens' DeleteIdentity Prelude.Text
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
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteIdentity where
  hashWithSalt _salt DeleteIdentity' {..} =
    _salt `Prelude.hashWithSalt` identity

instance Prelude.NFData DeleteIdentity where
  rnf DeleteIdentity' {..} = Prelude.rnf identity

instance Core.ToHeaders DeleteIdentity where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteIdentity where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteIdentity where
  toQuery DeleteIdentity' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DeleteIdentity" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-12-01" :: Prelude.ByteString),
        "Identity" Core.=: identity
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'newDeleteIdentityResponse' smart constructor.
data DeleteIdentityResponse = DeleteIdentityResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DeleteIdentityResponse
newDeleteIdentityResponse pHttpStatus_ =
  DeleteIdentityResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteIdentityResponse_httpStatus :: Lens.Lens' DeleteIdentityResponse Prelude.Int
deleteIdentityResponse_httpStatus = Lens.lens (\DeleteIdentityResponse' {httpStatus} -> httpStatus) (\s@DeleteIdentityResponse' {} a -> s {httpStatus = a} :: DeleteIdentityResponse)

instance Prelude.NFData DeleteIdentityResponse where
  rnf DeleteIdentityResponse' {..} =
    Prelude.rnf httpStatus
