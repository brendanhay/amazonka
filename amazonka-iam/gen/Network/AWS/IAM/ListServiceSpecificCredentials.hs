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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListServiceSpecificCredentials' smart constructor.
data ListServiceSpecificCredentials = ListServiceSpecificCredentials'
  { -- | Filters the returned results to only those for the specified AWS
    -- service. If not specified, then AWS returns service-specific credentials
    -- for all services.
    serviceName :: Core.Maybe Core.Text,
    -- | The name of the user whose service-specific credentials you want
    -- information about. If this value is not specified, then the operation
    -- assumes the user whose credentials are used to call the operation.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    userName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      userName = Core.Nothing
    }

-- | Filters the returned results to only those for the specified AWS
-- service. If not specified, then AWS returns service-specific credentials
-- for all services.
listServiceSpecificCredentials_serviceName :: Lens.Lens' ListServiceSpecificCredentials (Core.Maybe Core.Text)
listServiceSpecificCredentials_serviceName = Lens.lens (\ListServiceSpecificCredentials' {serviceName} -> serviceName) (\s@ListServiceSpecificCredentials' {} a -> s {serviceName = a} :: ListServiceSpecificCredentials)

-- | The name of the user whose service-specific credentials you want
-- information about. If this value is not specified, then the operation
-- assumes the user whose credentials are used to call the operation.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
listServiceSpecificCredentials_userName :: Lens.Lens' ListServiceSpecificCredentials (Core.Maybe Core.Text)
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
            Core.<$> ( x Core..@? "ServiceSpecificCredentials"
                         Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListServiceSpecificCredentials

instance Core.NFData ListServiceSpecificCredentials

instance
  Core.ToHeaders
    ListServiceSpecificCredentials
  where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListServiceSpecificCredentials where
  toPath = Core.const "/"

instance Core.ToQuery ListServiceSpecificCredentials where
  toQuery ListServiceSpecificCredentials' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "ListServiceSpecificCredentials" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2010-05-08" :: Core.ByteString),
        "ServiceName" Core.=: serviceName,
        "UserName" Core.=: userName
      ]

-- | /See:/ 'newListServiceSpecificCredentialsResponse' smart constructor.
data ListServiceSpecificCredentialsResponse = ListServiceSpecificCredentialsResponse'
  { -- | A list of structures that each contain details about a service-specific
    -- credential.
    serviceSpecificCredentials :: Core.Maybe [ServiceSpecificCredentialMetadata],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListServiceSpecificCredentialsResponse
newListServiceSpecificCredentialsResponse
  pHttpStatus_ =
    ListServiceSpecificCredentialsResponse'
      { serviceSpecificCredentials =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A list of structures that each contain details about a service-specific
-- credential.
listServiceSpecificCredentialsResponse_serviceSpecificCredentials :: Lens.Lens' ListServiceSpecificCredentialsResponse (Core.Maybe [ServiceSpecificCredentialMetadata])
listServiceSpecificCredentialsResponse_serviceSpecificCredentials = Lens.lens (\ListServiceSpecificCredentialsResponse' {serviceSpecificCredentials} -> serviceSpecificCredentials) (\s@ListServiceSpecificCredentialsResponse' {} a -> s {serviceSpecificCredentials = a} :: ListServiceSpecificCredentialsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listServiceSpecificCredentialsResponse_httpStatus :: Lens.Lens' ListServiceSpecificCredentialsResponse Core.Int
listServiceSpecificCredentialsResponse_httpStatus = Lens.lens (\ListServiceSpecificCredentialsResponse' {httpStatus} -> httpStatus) (\s@ListServiceSpecificCredentialsResponse' {} a -> s {httpStatus = a} :: ListServiceSpecificCredentialsResponse)

instance
  Core.NFData
    ListServiceSpecificCredentialsResponse
