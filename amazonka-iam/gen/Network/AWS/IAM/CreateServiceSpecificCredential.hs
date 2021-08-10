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
-- Module      : Network.AWS.IAM.CreateServiceSpecificCredential
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a set of credentials consisting of a user name and password
-- that can be used to access the service specified in the request. These
-- credentials are generated by IAM, and can be used only for the specified
-- service.
--
-- You can have a maximum of two sets of service-specific credentials for
-- each supported service per user.
--
-- You can create service-specific credentials for AWS CodeCommit and
-- Amazon Keyspaces (for Apache Cassandra).
--
-- You can reset the password to a new service-generated value by calling
-- ResetServiceSpecificCredential.
--
-- For more information about service-specific credentials, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_ssh-keys.html Using IAM with AWS CodeCommit: Git credentials, SSH keys, and AWS access keys>
-- in the /IAM User Guide/.
module Network.AWS.IAM.CreateServiceSpecificCredential
  ( -- * Creating a Request
    CreateServiceSpecificCredential (..),
    newCreateServiceSpecificCredential,

    -- * Request Lenses
    createServiceSpecificCredential_userName,
    createServiceSpecificCredential_serviceName,

    -- * Destructuring the Response
    CreateServiceSpecificCredentialResponse (..),
    newCreateServiceSpecificCredentialResponse,

    -- * Response Lenses
    createServiceSpecificCredentialResponse_serviceSpecificCredential,
    createServiceSpecificCredentialResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateServiceSpecificCredential' smart constructor.
data CreateServiceSpecificCredential = CreateServiceSpecificCredential'
  { -- | The name of the IAM user that is to be associated with the credentials.
    -- The new service-specific credentials have the same permissions as the
    -- associated user except that they can be used only to access the
    -- specified service.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    userName :: Prelude.Text,
    -- | The name of the AWS service that is to be associated with the
    -- credentials. The service you specify here is the only service that can
    -- be accessed using these credentials.
    serviceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateServiceSpecificCredential' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userName', 'createServiceSpecificCredential_userName' - The name of the IAM user that is to be associated with the credentials.
-- The new service-specific credentials have the same permissions as the
-- associated user except that they can be used only to access the
-- specified service.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
--
-- 'serviceName', 'createServiceSpecificCredential_serviceName' - The name of the AWS service that is to be associated with the
-- credentials. The service you specify here is the only service that can
-- be accessed using these credentials.
newCreateServiceSpecificCredential ::
  -- | 'userName'
  Prelude.Text ->
  -- | 'serviceName'
  Prelude.Text ->
  CreateServiceSpecificCredential
newCreateServiceSpecificCredential
  pUserName_
  pServiceName_ =
    CreateServiceSpecificCredential'
      { userName =
          pUserName_,
        serviceName = pServiceName_
      }

-- | The name of the IAM user that is to be associated with the credentials.
-- The new service-specific credentials have the same permissions as the
-- associated user except that they can be used only to access the
-- specified service.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
createServiceSpecificCredential_userName :: Lens.Lens' CreateServiceSpecificCredential Prelude.Text
createServiceSpecificCredential_userName = Lens.lens (\CreateServiceSpecificCredential' {userName} -> userName) (\s@CreateServiceSpecificCredential' {} a -> s {userName = a} :: CreateServiceSpecificCredential)

-- | The name of the AWS service that is to be associated with the
-- credentials. The service you specify here is the only service that can
-- be accessed using these credentials.
createServiceSpecificCredential_serviceName :: Lens.Lens' CreateServiceSpecificCredential Prelude.Text
createServiceSpecificCredential_serviceName = Lens.lens (\CreateServiceSpecificCredential' {serviceName} -> serviceName) (\s@CreateServiceSpecificCredential' {} a -> s {serviceName = a} :: CreateServiceSpecificCredential)

instance
  Core.AWSRequest
    CreateServiceSpecificCredential
  where
  type
    AWSResponse CreateServiceSpecificCredential =
      CreateServiceSpecificCredentialResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CreateServiceSpecificCredentialResult"
      ( \s h x ->
          CreateServiceSpecificCredentialResponse'
            Prelude.<$> (x Core..@? "ServiceSpecificCredential")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateServiceSpecificCredential

instance
  Prelude.NFData
    CreateServiceSpecificCredential

instance
  Core.ToHeaders
    CreateServiceSpecificCredential
  where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateServiceSpecificCredential where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateServiceSpecificCredential where
  toQuery CreateServiceSpecificCredential' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "CreateServiceSpecificCredential" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2010-05-08" :: Prelude.ByteString),
        "UserName" Core.=: userName,
        "ServiceName" Core.=: serviceName
      ]

-- | /See:/ 'newCreateServiceSpecificCredentialResponse' smart constructor.
data CreateServiceSpecificCredentialResponse = CreateServiceSpecificCredentialResponse'
  { -- | A structure that contains information about the newly created
    -- service-specific credential.
    --
    -- This is the only time that the password for this credential set is
    -- available. It cannot be recovered later. Instead, you must reset the
    -- password with ResetServiceSpecificCredential.
    serviceSpecificCredential :: Prelude.Maybe ServiceSpecificCredential,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateServiceSpecificCredentialResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceSpecificCredential', 'createServiceSpecificCredentialResponse_serviceSpecificCredential' - A structure that contains information about the newly created
-- service-specific credential.
--
-- This is the only time that the password for this credential set is
-- available. It cannot be recovered later. Instead, you must reset the
-- password with ResetServiceSpecificCredential.
--
-- 'httpStatus', 'createServiceSpecificCredentialResponse_httpStatus' - The response's http status code.
newCreateServiceSpecificCredentialResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateServiceSpecificCredentialResponse
newCreateServiceSpecificCredentialResponse
  pHttpStatus_ =
    CreateServiceSpecificCredentialResponse'
      { serviceSpecificCredential =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A structure that contains information about the newly created
-- service-specific credential.
--
-- This is the only time that the password for this credential set is
-- available. It cannot be recovered later. Instead, you must reset the
-- password with ResetServiceSpecificCredential.
createServiceSpecificCredentialResponse_serviceSpecificCredential :: Lens.Lens' CreateServiceSpecificCredentialResponse (Prelude.Maybe ServiceSpecificCredential)
createServiceSpecificCredentialResponse_serviceSpecificCredential = Lens.lens (\CreateServiceSpecificCredentialResponse' {serviceSpecificCredential} -> serviceSpecificCredential) (\s@CreateServiceSpecificCredentialResponse' {} a -> s {serviceSpecificCredential = a} :: CreateServiceSpecificCredentialResponse)

-- | The response's http status code.
createServiceSpecificCredentialResponse_httpStatus :: Lens.Lens' CreateServiceSpecificCredentialResponse Prelude.Int
createServiceSpecificCredentialResponse_httpStatus = Lens.lens (\CreateServiceSpecificCredentialResponse' {httpStatus} -> httpStatus) (\s@CreateServiceSpecificCredentialResponse' {} a -> s {httpStatus = a} :: CreateServiceSpecificCredentialResponse)

instance
  Prelude.NFData
    CreateServiceSpecificCredentialResponse
