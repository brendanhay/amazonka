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
-- Module      : Network.AWS.IAM.ResetServiceSpecificCredential
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets the password for a service-specific credential. The new password
-- is AWS generated and cryptographically strong. It cannot be configured
-- by the user. Resetting the password immediately invalidates the previous
-- password associated with this user.
module Network.AWS.IAM.ResetServiceSpecificCredential
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

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
    userName :: Core.Maybe Core.Text,
    -- | The unique identifier of the service-specific credential.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- that can consist of any upper or lowercased letter or digit.
    serviceSpecificCredentialId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  ResetServiceSpecificCredential
newResetServiceSpecificCredential
  pServiceSpecificCredentialId_ =
    ResetServiceSpecificCredential'
      { userName =
          Core.Nothing,
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
resetServiceSpecificCredential_userName :: Lens.Lens' ResetServiceSpecificCredential (Core.Maybe Core.Text)
resetServiceSpecificCredential_userName = Lens.lens (\ResetServiceSpecificCredential' {userName} -> userName) (\s@ResetServiceSpecificCredential' {} a -> s {userName = a} :: ResetServiceSpecificCredential)

-- | The unique identifier of the service-specific credential.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that can consist of any upper or lowercased letter or digit.
resetServiceSpecificCredential_serviceSpecificCredentialId :: Lens.Lens' ResetServiceSpecificCredential Core.Text
resetServiceSpecificCredential_serviceSpecificCredentialId = Lens.lens (\ResetServiceSpecificCredential' {serviceSpecificCredentialId} -> serviceSpecificCredentialId) (\s@ResetServiceSpecificCredential' {} a -> s {serviceSpecificCredentialId = a} :: ResetServiceSpecificCredential)

instance
  Core.AWSRequest
    ResetServiceSpecificCredential
  where
  type
    AWSResponse ResetServiceSpecificCredential =
      ResetServiceSpecificCredentialResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ResetServiceSpecificCredentialResult"
      ( \s h x ->
          ResetServiceSpecificCredentialResponse'
            Core.<$> (x Core..@? "ServiceSpecificCredential")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ResetServiceSpecificCredential

instance Core.NFData ResetServiceSpecificCredential

instance
  Core.ToHeaders
    ResetServiceSpecificCredential
  where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ResetServiceSpecificCredential where
  toPath = Core.const "/"

instance Core.ToQuery ResetServiceSpecificCredential where
  toQuery ResetServiceSpecificCredential' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "ResetServiceSpecificCredential" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2010-05-08" :: Core.ByteString),
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
    serviceSpecificCredential :: Core.Maybe ServiceSpecificCredential,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
  Core.Int ->
  ResetServiceSpecificCredentialResponse
newResetServiceSpecificCredentialResponse
  pHttpStatus_ =
    ResetServiceSpecificCredentialResponse'
      { serviceSpecificCredential =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A structure with details about the updated service-specific credential,
-- including the new password.
--
-- This is the __only__ time that you can access the password. You cannot
-- recover the password later, but you can reset it again.
resetServiceSpecificCredentialResponse_serviceSpecificCredential :: Lens.Lens' ResetServiceSpecificCredentialResponse (Core.Maybe ServiceSpecificCredential)
resetServiceSpecificCredentialResponse_serviceSpecificCredential = Lens.lens (\ResetServiceSpecificCredentialResponse' {serviceSpecificCredential} -> serviceSpecificCredential) (\s@ResetServiceSpecificCredentialResponse' {} a -> s {serviceSpecificCredential = a} :: ResetServiceSpecificCredentialResponse)

-- | The response's http status code.
resetServiceSpecificCredentialResponse_httpStatus :: Lens.Lens' ResetServiceSpecificCredentialResponse Core.Int
resetServiceSpecificCredentialResponse_httpStatus = Lens.lens (\ResetServiceSpecificCredentialResponse' {httpStatus} -> httpStatus) (\s@ResetServiceSpecificCredentialResponse' {} a -> s {httpStatus = a} :: ResetServiceSpecificCredentialResponse)

instance
  Core.NFData
    ResetServiceSpecificCredentialResponse
