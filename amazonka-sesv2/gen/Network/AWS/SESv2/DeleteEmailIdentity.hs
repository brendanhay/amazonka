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
-- Module      : Network.AWS.SESv2.DeleteEmailIdentity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an email identity. An identity can be either an email address or
-- a domain name.
module Network.AWS.SESv2.DeleteEmailIdentity
  ( -- * Creating a Request
    DeleteEmailIdentity (..),
    newDeleteEmailIdentity,

    -- * Request Lenses
    deleteEmailIdentity_emailIdentity,

    -- * Destructuring the Response
    DeleteEmailIdentityResponse (..),
    newDeleteEmailIdentityResponse,

    -- * Response Lenses
    deleteEmailIdentityResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SESv2.Types

-- | A request to delete an existing email identity. When you delete an
-- identity, you lose the ability to send email from that identity. You can
-- restore your ability to send email by completing the verification
-- process for the identity again.
--
-- /See:/ 'newDeleteEmailIdentity' smart constructor.
data DeleteEmailIdentity = DeleteEmailIdentity'
  { -- | The identity (that is, the email address or domain) that you want to
    -- delete.
    emailIdentity :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEmailIdentity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'emailIdentity', 'deleteEmailIdentity_emailIdentity' - The identity (that is, the email address or domain) that you want to
-- delete.
newDeleteEmailIdentity ::
  -- | 'emailIdentity'
  Prelude.Text ->
  DeleteEmailIdentity
newDeleteEmailIdentity pEmailIdentity_ =
  DeleteEmailIdentity'
    { emailIdentity =
        pEmailIdentity_
    }

-- | The identity (that is, the email address or domain) that you want to
-- delete.
deleteEmailIdentity_emailIdentity :: Lens.Lens' DeleteEmailIdentity Prelude.Text
deleteEmailIdentity_emailIdentity = Lens.lens (\DeleteEmailIdentity' {emailIdentity} -> emailIdentity) (\s@DeleteEmailIdentity' {} a -> s {emailIdentity = a} :: DeleteEmailIdentity)

instance Core.AWSRequest DeleteEmailIdentity where
  type
    AWSResponse DeleteEmailIdentity =
      DeleteEmailIdentityResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteEmailIdentityResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteEmailIdentity

instance Prelude.NFData DeleteEmailIdentity

instance Core.ToHeaders DeleteEmailIdentity where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteEmailIdentity where
  toPath DeleteEmailIdentity' {..} =
    Prelude.mconcat
      ["/v2/email/identities/", Core.toBS emailIdentity]

instance Core.ToQuery DeleteEmailIdentity where
  toQuery = Prelude.const Prelude.mempty

-- | An HTTP 200 response if the request succeeds, or an error message if the
-- request fails.
--
-- /See:/ 'newDeleteEmailIdentityResponse' smart constructor.
data DeleteEmailIdentityResponse = DeleteEmailIdentityResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEmailIdentityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteEmailIdentityResponse_httpStatus' - The response's http status code.
newDeleteEmailIdentityResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteEmailIdentityResponse
newDeleteEmailIdentityResponse pHttpStatus_ =
  DeleteEmailIdentityResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteEmailIdentityResponse_httpStatus :: Lens.Lens' DeleteEmailIdentityResponse Prelude.Int
deleteEmailIdentityResponse_httpStatus = Lens.lens (\DeleteEmailIdentityResponse' {httpStatus} -> httpStatus) (\s@DeleteEmailIdentityResponse' {} a -> s {httpStatus = a} :: DeleteEmailIdentityResponse)

instance Prelude.NFData DeleteEmailIdentityResponse
