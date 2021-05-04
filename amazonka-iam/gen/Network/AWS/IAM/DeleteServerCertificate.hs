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
-- Module      : Network.AWS.IAM.DeleteServerCertificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified server certificate.
--
-- For more information about working with server certificates, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_server-certs.html Working with server certificates>
-- in the /IAM User Guide/. This topic also includes a list of AWS services
-- that can use the server certificates that you manage with IAM.
--
-- If you are using a server certificate with Elastic Load Balancing,
-- deleting the certificate could have implications for your application.
-- If Elastic Load Balancing doesn\'t detect the deletion of bound
-- certificates, it may continue to use the certificates. This could cause
-- Elastic Load Balancing to stop accepting traffic. We recommend that you
-- remove the reference to the certificate from Elastic Load Balancing
-- before using this command to delete the certificate. For more
-- information, see
-- <https://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_DeleteLoadBalancerListeners.html DeleteLoadBalancerListeners>
-- in the /Elastic Load Balancing API Reference/.
module Network.AWS.IAM.DeleteServerCertificate
  ( -- * Creating a Request
    DeleteServerCertificate (..),
    newDeleteServerCertificate,

    -- * Request Lenses
    deleteServerCertificate_serverCertificateName,

    -- * Destructuring the Response
    DeleteServerCertificateResponse (..),
    newDeleteServerCertificateResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteServerCertificate' smart constructor.
data DeleteServerCertificate = DeleteServerCertificate'
  { -- | The name of the server certificate you want to delete.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    serverCertificateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteServerCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serverCertificateName', 'deleteServerCertificate_serverCertificateName' - The name of the server certificate you want to delete.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
newDeleteServerCertificate ::
  -- | 'serverCertificateName'
  Prelude.Text ->
  DeleteServerCertificate
newDeleteServerCertificate pServerCertificateName_ =
  DeleteServerCertificate'
    { serverCertificateName =
        pServerCertificateName_
    }

-- | The name of the server certificate you want to delete.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
deleteServerCertificate_serverCertificateName :: Lens.Lens' DeleteServerCertificate Prelude.Text
deleteServerCertificate_serverCertificateName = Lens.lens (\DeleteServerCertificate' {serverCertificateName} -> serverCertificateName) (\s@DeleteServerCertificate' {} a -> s {serverCertificateName = a} :: DeleteServerCertificate)

instance Prelude.AWSRequest DeleteServerCertificate where
  type
    Rs DeleteServerCertificate =
      DeleteServerCertificateResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      DeleteServerCertificateResponse'

instance Prelude.Hashable DeleteServerCertificate

instance Prelude.NFData DeleteServerCertificate

instance Prelude.ToHeaders DeleteServerCertificate where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteServerCertificate where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteServerCertificate where
  toQuery DeleteServerCertificate' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DeleteServerCertificate" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-08" :: Prelude.ByteString),
        "ServerCertificateName"
          Prelude.=: serverCertificateName
      ]

-- | /See:/ 'newDeleteServerCertificateResponse' smart constructor.
data DeleteServerCertificateResponse = DeleteServerCertificateResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteServerCertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteServerCertificateResponse ::
  DeleteServerCertificateResponse
newDeleteServerCertificateResponse =
  DeleteServerCertificateResponse'

instance
  Prelude.NFData
    DeleteServerCertificateResponse
