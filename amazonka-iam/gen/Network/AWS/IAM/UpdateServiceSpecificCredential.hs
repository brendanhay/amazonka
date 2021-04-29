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
-- Module      : Network.AWS.IAM.UpdateServiceSpecificCredential
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the status of a service-specific credential to @Active@ or
-- @Inactive@. Service-specific credentials that are inactive cannot be
-- used for authentication to the service. This operation can be used to
-- disable a user\'s service-specific credential as part of a credential
-- rotation work flow.
module Network.AWS.IAM.UpdateServiceSpecificCredential
  ( -- * Creating a Request
    UpdateServiceSpecificCredential (..),
    newUpdateServiceSpecificCredential,

    -- * Request Lenses
    updateServiceSpecificCredential_userName,
    updateServiceSpecificCredential_serviceSpecificCredentialId,
    updateServiceSpecificCredential_status,

    -- * Destructuring the Response
    UpdateServiceSpecificCredentialResponse (..),
    newUpdateServiceSpecificCredentialResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateServiceSpecificCredential' smart constructor.
data UpdateServiceSpecificCredential = UpdateServiceSpecificCredential'
  { -- | The name of the IAM user associated with the service-specific
    -- credential. If you do not specify this value, then the operation assumes
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
    serviceSpecificCredentialId :: Prelude.Text,
    -- | The status to be assigned to the service-specific credential.
    status :: StatusType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateServiceSpecificCredential' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userName', 'updateServiceSpecificCredential_userName' - The name of the IAM user associated with the service-specific
-- credential. If you do not specify this value, then the operation assumes
-- the user whose credentials are used to call the operation.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
--
-- 'serviceSpecificCredentialId', 'updateServiceSpecificCredential_serviceSpecificCredentialId' - The unique identifier of the service-specific credential.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that can consist of any upper or lowercased letter or digit.
--
-- 'status', 'updateServiceSpecificCredential_status' - The status to be assigned to the service-specific credential.
newUpdateServiceSpecificCredential ::
  -- | 'serviceSpecificCredentialId'
  Prelude.Text ->
  -- | 'status'
  StatusType ->
  UpdateServiceSpecificCredential
newUpdateServiceSpecificCredential
  pServiceSpecificCredentialId_
  pStatus_ =
    UpdateServiceSpecificCredential'
      { userName =
          Prelude.Nothing,
        serviceSpecificCredentialId =
          pServiceSpecificCredentialId_,
        status = pStatus_
      }

-- | The name of the IAM user associated with the service-specific
-- credential. If you do not specify this value, then the operation assumes
-- the user whose credentials are used to call the operation.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
updateServiceSpecificCredential_userName :: Lens.Lens' UpdateServiceSpecificCredential (Prelude.Maybe Prelude.Text)
updateServiceSpecificCredential_userName = Lens.lens (\UpdateServiceSpecificCredential' {userName} -> userName) (\s@UpdateServiceSpecificCredential' {} a -> s {userName = a} :: UpdateServiceSpecificCredential)

-- | The unique identifier of the service-specific credential.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that can consist of any upper or lowercased letter or digit.
updateServiceSpecificCredential_serviceSpecificCredentialId :: Lens.Lens' UpdateServiceSpecificCredential Prelude.Text
updateServiceSpecificCredential_serviceSpecificCredentialId = Lens.lens (\UpdateServiceSpecificCredential' {serviceSpecificCredentialId} -> serviceSpecificCredentialId) (\s@UpdateServiceSpecificCredential' {} a -> s {serviceSpecificCredentialId = a} :: UpdateServiceSpecificCredential)

-- | The status to be assigned to the service-specific credential.
updateServiceSpecificCredential_status :: Lens.Lens' UpdateServiceSpecificCredential StatusType
updateServiceSpecificCredential_status = Lens.lens (\UpdateServiceSpecificCredential' {status} -> status) (\s@UpdateServiceSpecificCredential' {} a -> s {status = a} :: UpdateServiceSpecificCredential)

instance
  Prelude.AWSRequest
    UpdateServiceSpecificCredential
  where
  type
    Rs UpdateServiceSpecificCredential =
      UpdateServiceSpecificCredentialResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      UpdateServiceSpecificCredentialResponse'

instance
  Prelude.Hashable
    UpdateServiceSpecificCredential

instance
  Prelude.NFData
    UpdateServiceSpecificCredential

instance
  Prelude.ToHeaders
    UpdateServiceSpecificCredential
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToPath
    UpdateServiceSpecificCredential
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    UpdateServiceSpecificCredential
  where
  toQuery UpdateServiceSpecificCredential' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "UpdateServiceSpecificCredential" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2010-05-08" :: Prelude.ByteString),
        "UserName" Prelude.=: userName,
        "ServiceSpecificCredentialId"
          Prelude.=: serviceSpecificCredentialId,
        "Status" Prelude.=: status
      ]

-- | /See:/ 'newUpdateServiceSpecificCredentialResponse' smart constructor.
data UpdateServiceSpecificCredentialResponse = UpdateServiceSpecificCredentialResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateServiceSpecificCredentialResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateServiceSpecificCredentialResponse ::
  UpdateServiceSpecificCredentialResponse
newUpdateServiceSpecificCredentialResponse =
  UpdateServiceSpecificCredentialResponse'

instance
  Prelude.NFData
    UpdateServiceSpecificCredentialResponse
