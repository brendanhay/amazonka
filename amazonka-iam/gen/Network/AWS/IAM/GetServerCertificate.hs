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
-- Module      : Network.AWS.IAM.GetServerCertificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the specified server certificate stored in
-- IAM.
--
-- For more information about working with server certificates, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_server-certs.html Working with server certificates>
-- in the /IAM User Guide/. This topic includes a list of AWS services that
-- can use the server certificates that you manage with IAM.
module Network.AWS.IAM.GetServerCertificate
  ( -- * Creating a Request
    GetServerCertificate (..),
    newGetServerCertificate,

    -- * Request Lenses
    getServerCertificate_serverCertificateName,

    -- * Destructuring the Response
    GetServerCertificateResponse (..),
    newGetServerCertificateResponse,

    -- * Response Lenses
    getServerCertificateResponse_httpStatus,
    getServerCertificateResponse_serverCertificate,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetServerCertificate' smart constructor.
data GetServerCertificate = GetServerCertificate'
  { -- | The name of the server certificate you want to retrieve information
    -- about.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    serverCertificateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetServerCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serverCertificateName', 'getServerCertificate_serverCertificateName' - The name of the server certificate you want to retrieve information
-- about.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
newGetServerCertificate ::
  -- | 'serverCertificateName'
  Prelude.Text ->
  GetServerCertificate
newGetServerCertificate pServerCertificateName_ =
  GetServerCertificate'
    { serverCertificateName =
        pServerCertificateName_
    }

-- | The name of the server certificate you want to retrieve information
-- about.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
getServerCertificate_serverCertificateName :: Lens.Lens' GetServerCertificate Prelude.Text
getServerCertificate_serverCertificateName = Lens.lens (\GetServerCertificate' {serverCertificateName} -> serverCertificateName) (\s@GetServerCertificate' {} a -> s {serverCertificateName = a} :: GetServerCertificate)

instance Core.AWSRequest GetServerCertificate where
  type
    AWSResponse GetServerCertificate =
      GetServerCertificateResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "GetServerCertificateResult"
      ( \s h x ->
          GetServerCertificateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..@ "ServerCertificate")
      )

instance Prelude.Hashable GetServerCertificate

instance Prelude.NFData GetServerCertificate

instance Core.ToHeaders GetServerCertificate where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetServerCertificate where
  toPath = Prelude.const "/"

instance Core.ToQuery GetServerCertificate where
  toQuery GetServerCertificate' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("GetServerCertificate" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-05-08" :: Prelude.ByteString),
        "ServerCertificateName"
          Core.=: serverCertificateName
      ]

-- | Contains the response to a successful GetServerCertificate request.
--
-- /See:/ 'newGetServerCertificateResponse' smart constructor.
data GetServerCertificateResponse = GetServerCertificateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A structure containing details about the server certificate.
    serverCertificate :: ServerCertificate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetServerCertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getServerCertificateResponse_httpStatus' - The response's http status code.
--
-- 'serverCertificate', 'getServerCertificateResponse_serverCertificate' - A structure containing details about the server certificate.
newGetServerCertificateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'serverCertificate'
  ServerCertificate ->
  GetServerCertificateResponse
newGetServerCertificateResponse
  pHttpStatus_
  pServerCertificate_ =
    GetServerCertificateResponse'
      { httpStatus =
          pHttpStatus_,
        serverCertificate = pServerCertificate_
      }

-- | The response's http status code.
getServerCertificateResponse_httpStatus :: Lens.Lens' GetServerCertificateResponse Prelude.Int
getServerCertificateResponse_httpStatus = Lens.lens (\GetServerCertificateResponse' {httpStatus} -> httpStatus) (\s@GetServerCertificateResponse' {} a -> s {httpStatus = a} :: GetServerCertificateResponse)

-- | A structure containing details about the server certificate.
getServerCertificateResponse_serverCertificate :: Lens.Lens' GetServerCertificateResponse ServerCertificate
getServerCertificateResponse_serverCertificate = Lens.lens (\GetServerCertificateResponse' {serverCertificate} -> serverCertificate) (\s@GetServerCertificateResponse' {} a -> s {serverCertificate = a} :: GetServerCertificateResponse)

instance Prelude.NFData GetServerCertificateResponse
