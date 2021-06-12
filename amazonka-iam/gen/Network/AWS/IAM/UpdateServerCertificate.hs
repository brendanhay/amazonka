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
-- Module      : Network.AWS.IAM.UpdateServerCertificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the name and\/or the path of the specified server certificate
-- stored in IAM.
--
-- For more information about working with server certificates, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_server-certs.html Working with server certificates>
-- in the /IAM User Guide/. This topic also includes a list of AWS services
-- that can use the server certificates that you manage with IAM.
--
-- You should understand the implications of changing a server
-- certificate\'s path or name. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_server-certs_manage.html#RenamingServerCerts Renaming a server certificate>
-- in the /IAM User Guide/.
--
-- The person making the request (the principal), must have permission to
-- change the server certificate with the old name and the new name. For
-- example, to change the certificate named @ProductionCert@ to @ProdCert@,
-- the principal must have a policy that allows them to update both
-- certificates. If the principal has permission to update the
-- @ProductionCert@ group, but not the @ProdCert@ certificate, then the
-- update fails. For more information about permissions, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access.html Access management>
-- in the /IAM User Guide/.
module Network.AWS.IAM.UpdateServerCertificate
  ( -- * Creating a Request
    UpdateServerCertificate (..),
    newUpdateServerCertificate,

    -- * Request Lenses
    updateServerCertificate_newPath,
    updateServerCertificate_newServerCertificateName,
    updateServerCertificate_serverCertificateName,

    -- * Destructuring the Response
    UpdateServerCertificateResponse (..),
    newUpdateServerCertificateResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateServerCertificate' smart constructor.
data UpdateServerCertificate = UpdateServerCertificate'
  { -- | The new path for the server certificate. Include this only if you are
    -- updating the server certificate\'s path.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of either a forward slash (\/) by itself or a string that
    -- must begin and end with forward slashes. In addition, it can contain any
    -- ASCII character from the ! (@\\u0021@) through the DEL character
    -- (@\\u007F@), including most punctuation characters, digits, and upper
    -- and lowercased letters.
    newPath' :: Core.Maybe Core.Text,
    -- | The new name for the server certificate. Include this only if you are
    -- updating the server certificate\'s name. The name of the certificate
    -- cannot contain any spaces.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    newServerCertificateName' :: Core.Maybe Core.Text,
    -- | The name of the server certificate that you want to update.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    serverCertificateName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateServerCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'newPath'', 'updateServerCertificate_newPath' - The new path for the server certificate. Include this only if you are
-- updating the server certificate\'s path.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of either a forward slash (\/) by itself or a string that
-- must begin and end with forward slashes. In addition, it can contain any
-- ASCII character from the ! (@\\u0021@) through the DEL character
-- (@\\u007F@), including most punctuation characters, digits, and upper
-- and lowercased letters.
--
-- 'newServerCertificateName'', 'updateServerCertificate_newServerCertificateName' - The new name for the server certificate. Include this only if you are
-- updating the server certificate\'s name. The name of the certificate
-- cannot contain any spaces.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
--
-- 'serverCertificateName', 'updateServerCertificate_serverCertificateName' - The name of the server certificate that you want to update.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
newUpdateServerCertificate ::
  -- | 'serverCertificateName'
  Core.Text ->
  UpdateServerCertificate
newUpdateServerCertificate pServerCertificateName_ =
  UpdateServerCertificate'
    { newPath' = Core.Nothing,
      newServerCertificateName' = Core.Nothing,
      serverCertificateName = pServerCertificateName_
    }

-- | The new path for the server certificate. Include this only if you are
-- updating the server certificate\'s path.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of either a forward slash (\/) by itself or a string that
-- must begin and end with forward slashes. In addition, it can contain any
-- ASCII character from the ! (@\\u0021@) through the DEL character
-- (@\\u007F@), including most punctuation characters, digits, and upper
-- and lowercased letters.
updateServerCertificate_newPath :: Lens.Lens' UpdateServerCertificate (Core.Maybe Core.Text)
updateServerCertificate_newPath = Lens.lens (\UpdateServerCertificate' {newPath'} -> newPath') (\s@UpdateServerCertificate' {} a -> s {newPath' = a} :: UpdateServerCertificate)

-- | The new name for the server certificate. Include this only if you are
-- updating the server certificate\'s name. The name of the certificate
-- cannot contain any spaces.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
updateServerCertificate_newServerCertificateName :: Lens.Lens' UpdateServerCertificate (Core.Maybe Core.Text)
updateServerCertificate_newServerCertificateName = Lens.lens (\UpdateServerCertificate' {newServerCertificateName'} -> newServerCertificateName') (\s@UpdateServerCertificate' {} a -> s {newServerCertificateName' = a} :: UpdateServerCertificate)

-- | The name of the server certificate that you want to update.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
updateServerCertificate_serverCertificateName :: Lens.Lens' UpdateServerCertificate Core.Text
updateServerCertificate_serverCertificateName = Lens.lens (\UpdateServerCertificate' {serverCertificateName} -> serverCertificateName) (\s@UpdateServerCertificate' {} a -> s {serverCertificateName = a} :: UpdateServerCertificate)

instance Core.AWSRequest UpdateServerCertificate where
  type
    AWSResponse UpdateServerCertificate =
      UpdateServerCertificateResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      UpdateServerCertificateResponse'

instance Core.Hashable UpdateServerCertificate

instance Core.NFData UpdateServerCertificate

instance Core.ToHeaders UpdateServerCertificate where
  toHeaders = Core.const Core.mempty

instance Core.ToPath UpdateServerCertificate where
  toPath = Core.const "/"

instance Core.ToQuery UpdateServerCertificate where
  toQuery UpdateServerCertificate' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("UpdateServerCertificate" :: Core.ByteString),
        "Version" Core.=: ("2010-05-08" :: Core.ByteString),
        "NewPath" Core.=: newPath',
        "NewServerCertificateName"
          Core.=: newServerCertificateName',
        "ServerCertificateName"
          Core.=: serverCertificateName
      ]

-- | /See:/ 'newUpdateServerCertificateResponse' smart constructor.
data UpdateServerCertificateResponse = UpdateServerCertificateResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateServerCertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateServerCertificateResponse ::
  UpdateServerCertificateResponse
newUpdateServerCertificateResponse =
  UpdateServerCertificateResponse'

instance Core.NFData UpdateServerCertificateResponse
