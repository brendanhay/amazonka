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
-- Module      : Network.AWS.IAM.ListServiceSpecificCredentials
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the service-specific credentials associated
-- with the specified IAM user. If none exists, the operation returns an
-- empty list. The service-specific credentials returned by this operation
-- are used only for authenticating the IAM user to a specific service. For
-- more information about using service-specific credentials to
-- authenticate to an AWS service, see
-- <https://docs.aws.amazon.com/codecommit/latest/userguide/setting-up-gc.html Set up service-specific credentials>
-- in the AWS CodeCommit User Guide.
module Network.AWS.IAM.ListServiceSpecificCredentials
  ( -- * Creating a Request
    ListServiceSpecificCredentials (..),
    newListServiceSpecificCredentials,

    -- * Request Lenses
    listServiceSpecificCredentials_serviceName,
    listServiceSpecificCredentials_userName,

    -- * Destructuring the Response
    ListServiceSpecificCredentialsResponse (..),
    newListServiceSpecificCredentialsResponse,

    -- * Response Lenses
    listServiceSpecificCredentialsResponse_serviceSpecificCredentials,
    listServiceSpecificCredentialsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListServiceSpecificCredentials' smart constructor.
data ListServiceSpecificCredentials = ListServiceSpecificCredentials'
  { -- | Filters the returned results to only those for the specified AWS
    -- service. If not specified, then AWS returns service-specific credentials
    -- for all services.
    serviceName :: Prelude.Maybe Prelude.Text,
    -- | The name of the user whose service-specific credentials you want
    -- information about. If this value is not specified, then the operation
    -- assumes the user whose credentials are used to call the operation.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    userName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListServiceSpecificCredentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceName', 'listServiceSpecificCredentials_serviceName' - Filters the returned results to only those for the specified AWS
-- service. If not specified, then AWS returns service-specific credentials
-- for all services.
--
-- 'userName', 'listServiceSpecificCredentials_userName' - The name of the user whose service-specific credentials you want
-- information about. If this value is not specified, then the operation
-- assumes the user whose credentials are used to call the operation.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
newListServiceSpecificCredentials ::
  ListServiceSpecificCredentials
newListServiceSpecificCredentials =
  ListServiceSpecificCredentials'
    { serviceName =
        Prelude.Nothing,
      userName = Prelude.Nothing
    }

-- | Filters the returned results to only those for the specified AWS
-- service. If not specified, then AWS returns service-specific credentials
-- for all services.
listServiceSpecificCredentials_serviceName :: Lens.Lens' ListServiceSpecificCredentials (Prelude.Maybe Prelude.Text)
listServiceSpecificCredentials_serviceName = Lens.lens (\ListServiceSpecificCredentials' {serviceName} -> serviceName) (\s@ListServiceSpecificCredentials' {} a -> s {serviceName = a} :: ListServiceSpecificCredentials)

-- | The name of the user whose service-specific credentials you want
-- information about. If this value is not specified, then the operation
-- assumes the user whose credentials are used to call the operation.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
listServiceSpecificCredentials_userName :: Lens.Lens' ListServiceSpecificCredentials (Prelude.Maybe Prelude.Text)
listServiceSpecificCredentials_userName = Lens.lens (\ListServiceSpecificCredentials' {userName} -> userName) (\s@ListServiceSpecificCredentials' {} a -> s {userName = a} :: ListServiceSpecificCredentials)

instance
  Core.AWSRequest
    ListServiceSpecificCredentials
  where
  type
    AWSResponse ListServiceSpecificCredentials =
      ListServiceSpecificCredentialsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListServiceSpecificCredentialsResult"
      ( \s h x ->
          ListServiceSpecificCredentialsResponse'
            Prelude.<$> ( x Core..@? "ServiceSpecificCredentials"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListServiceSpecificCredentials

instance
  Prelude.NFData
    ListServiceSpecificCredentials

instance
  Core.ToHeaders
    ListServiceSpecificCredentials
  where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListServiceSpecificCredentials where
  toPath = Prelude.const "/"

instance Core.ToQuery ListServiceSpecificCredentials where
  toQuery ListServiceSpecificCredentials' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "ListServiceSpecificCredentials" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2010-05-08" :: Prelude.ByteString),
        "ServiceName" Core.=: serviceName,
        "UserName" Core.=: userName
      ]

-- | /See:/ 'newListServiceSpecificCredentialsResponse' smart constructor.
data ListServiceSpecificCredentialsResponse = ListServiceSpecificCredentialsResponse'
  { -- | A list of structures that each contain details about a service-specific
    -- credential.
    serviceSpecificCredentials :: Prelude.Maybe [ServiceSpecificCredentialMetadata],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListServiceSpecificCredentialsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceSpecificCredentials', 'listServiceSpecificCredentialsResponse_serviceSpecificCredentials' - A list of structures that each contain details about a service-specific
-- credential.
--
-- 'httpStatus', 'listServiceSpecificCredentialsResponse_httpStatus' - The response's http status code.
newListServiceSpecificCredentialsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListServiceSpecificCredentialsResponse
newListServiceSpecificCredentialsResponse
  pHttpStatus_ =
    ListServiceSpecificCredentialsResponse'
      { serviceSpecificCredentials =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A list of structures that each contain details about a service-specific
-- credential.
listServiceSpecificCredentialsResponse_serviceSpecificCredentials :: Lens.Lens' ListServiceSpecificCredentialsResponse (Prelude.Maybe [ServiceSpecificCredentialMetadata])
listServiceSpecificCredentialsResponse_serviceSpecificCredentials = Lens.lens (\ListServiceSpecificCredentialsResponse' {serviceSpecificCredentials} -> serviceSpecificCredentials) (\s@ListServiceSpecificCredentialsResponse' {} a -> s {serviceSpecificCredentials = a} :: ListServiceSpecificCredentialsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listServiceSpecificCredentialsResponse_httpStatus :: Lens.Lens' ListServiceSpecificCredentialsResponse Prelude.Int
listServiceSpecificCredentialsResponse_httpStatus = Lens.lens (\ListServiceSpecificCredentialsResponse' {httpStatus} -> httpStatus) (\s@ListServiceSpecificCredentialsResponse' {} a -> s {httpStatus = a} :: ListServiceSpecificCredentialsResponse)

instance
  Prelude.NFData
    ListServiceSpecificCredentialsResponse
