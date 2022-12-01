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
-- Module      : Amazonka.IAM.ResetServiceSpecificCredential
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets the password for a service-specific credential. The new password
-- is Amazon Web Services generated and cryptographically strong. It cannot
-- be configured by the user. Resetting the password immediately
-- invalidates the previous password associated with this user.
module Amazonka.IAM.ResetServiceSpecificCredential
  ( -- * Creating a Request
    ResetServiceSpecificCredential (..),
    newResetServiceSpecificCredential,

    -- * Request Lenses
    resetServiceSpecificCredential_userName,
    resetServiceSpecificCredential_serviceSpecificCredentialId,

    -- * Destructuring the Response
    ResetServiceSpecificCredentialResponse (..),
    newResetServiceSpecificCredentialResponse,

    -- * Response Lenses
    resetServiceSpecificCredentialResponse_serviceSpecificCredential,
    resetServiceSpecificCredentialResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newResetServiceSpecificCredential' smart constructor.
data ResetServiceSpecificCredential = ResetServiceSpecificCredential'
  { -- | The name of the IAM user associated with the service-specific
    -- credential. If this value is not specified, then the operation assumes
    -- the user whose credentials are used to call the operation.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    userName :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the service-specific credential.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- that can consist of any upper or lowercased letter or digit.
    serviceSpecificCredentialId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResetServiceSpecificCredential' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userName', 'resetServiceSpecificCredential_userName' - The name of the IAM user associated with the service-specific
-- credential. If this value is not specified, then the operation assumes
-- the user whose credentials are used to call the operation.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
--
-- 'serviceSpecificCredentialId', 'resetServiceSpecificCredential_serviceSpecificCredentialId' - The unique identifier of the service-specific credential.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that can consist of any upper or lowercased letter or digit.
newResetServiceSpecificCredential ::
  -- | 'serviceSpecificCredentialId'
  Prelude.Text ->
  ResetServiceSpecificCredential
newResetServiceSpecificCredential
  pServiceSpecificCredentialId_ =
    ResetServiceSpecificCredential'
      { userName =
          Prelude.Nothing,
        serviceSpecificCredentialId =
          pServiceSpecificCredentialId_
      }

-- | The name of the IAM user associated with the service-specific
-- credential. If this value is not specified, then the operation assumes
-- the user whose credentials are used to call the operation.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
resetServiceSpecificCredential_userName :: Lens.Lens' ResetServiceSpecificCredential (Prelude.Maybe Prelude.Text)
resetServiceSpecificCredential_userName = Lens.lens (\ResetServiceSpecificCredential' {userName} -> userName) (\s@ResetServiceSpecificCredential' {} a -> s {userName = a} :: ResetServiceSpecificCredential)

-- | The unique identifier of the service-specific credential.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that can consist of any upper or lowercased letter or digit.
resetServiceSpecificCredential_serviceSpecificCredentialId :: Lens.Lens' ResetServiceSpecificCredential Prelude.Text
resetServiceSpecificCredential_serviceSpecificCredentialId = Lens.lens (\ResetServiceSpecificCredential' {serviceSpecificCredentialId} -> serviceSpecificCredentialId) (\s@ResetServiceSpecificCredential' {} a -> s {serviceSpecificCredentialId = a} :: ResetServiceSpecificCredential)

instance
  Core.AWSRequest
    ResetServiceSpecificCredential
  where
  type
    AWSResponse ResetServiceSpecificCredential =
      ResetServiceSpecificCredentialResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ResetServiceSpecificCredentialResult"
      ( \s h x ->
          ResetServiceSpecificCredentialResponse'
            Prelude.<$> (x Core..@? "ServiceSpecificCredential")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ResetServiceSpecificCredential
  where
  hashWithSalt
    _salt
    ResetServiceSpecificCredential' {..} =
      _salt `Prelude.hashWithSalt` userName
        `Prelude.hashWithSalt` serviceSpecificCredentialId

instance
  Prelude.NFData
    ResetServiceSpecificCredential
  where
  rnf ResetServiceSpecificCredential' {..} =
    Prelude.rnf userName
      `Prelude.seq` Prelude.rnf serviceSpecificCredentialId

instance
  Core.ToHeaders
    ResetServiceSpecificCredential
  where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ResetServiceSpecificCredential where
  toPath = Prelude.const "/"

instance Core.ToQuery ResetServiceSpecificCredential where
  toQuery ResetServiceSpecificCredential' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "ResetServiceSpecificCredential" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2010-05-08" :: Prelude.ByteString),
        "UserName" Core.=: userName,
        "ServiceSpecificCredentialId"
          Core.=: serviceSpecificCredentialId
      ]

-- | /See:/ 'newResetServiceSpecificCredentialResponse' smart constructor.
data ResetServiceSpecificCredentialResponse = ResetServiceSpecificCredentialResponse'
  { -- | A structure with details about the updated service-specific credential,
    -- including the new password.
    --
    -- This is the __only__ time that you can access the password. You cannot
    -- recover the password later, but you can reset it again.
    serviceSpecificCredential :: Prelude.Maybe ServiceSpecificCredential,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResetServiceSpecificCredentialResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceSpecificCredential', 'resetServiceSpecificCredentialResponse_serviceSpecificCredential' - A structure with details about the updated service-specific credential,
-- including the new password.
--
-- This is the __only__ time that you can access the password. You cannot
-- recover the password later, but you can reset it again.
--
-- 'httpStatus', 'resetServiceSpecificCredentialResponse_httpStatus' - The response's http status code.
newResetServiceSpecificCredentialResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ResetServiceSpecificCredentialResponse
newResetServiceSpecificCredentialResponse
  pHttpStatus_ =
    ResetServiceSpecificCredentialResponse'
      { serviceSpecificCredential =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A structure with details about the updated service-specific credential,
-- including the new password.
--
-- This is the __only__ time that you can access the password. You cannot
-- recover the password later, but you can reset it again.
resetServiceSpecificCredentialResponse_serviceSpecificCredential :: Lens.Lens' ResetServiceSpecificCredentialResponse (Prelude.Maybe ServiceSpecificCredential)
resetServiceSpecificCredentialResponse_serviceSpecificCredential = Lens.lens (\ResetServiceSpecificCredentialResponse' {serviceSpecificCredential} -> serviceSpecificCredential) (\s@ResetServiceSpecificCredentialResponse' {} a -> s {serviceSpecificCredential = a} :: ResetServiceSpecificCredentialResponse)

-- | The response's http status code.
resetServiceSpecificCredentialResponse_httpStatus :: Lens.Lens' ResetServiceSpecificCredentialResponse Prelude.Int
resetServiceSpecificCredentialResponse_httpStatus = Lens.lens (\ResetServiceSpecificCredentialResponse' {httpStatus} -> httpStatus) (\s@ResetServiceSpecificCredentialResponse' {} a -> s {httpStatus = a} :: ResetServiceSpecificCredentialResponse)

instance
  Prelude.NFData
    ResetServiceSpecificCredentialResponse
  where
  rnf ResetServiceSpecificCredentialResponse' {..} =
    Prelude.rnf serviceSpecificCredential
      `Prelude.seq` Prelude.rnf httpStatus
