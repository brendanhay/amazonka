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

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    newPath' :: Prelude.Maybe Prelude.Text,
    -- | The new name for the server certificate. Include this only if you are
    -- updating the server certificate\'s name. The name of the certificate
    -- cannot contain any spaces.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    newServerCertificateName' :: Prelude.Maybe Prelude.Text,
    -- | The name of the server certificate that you want to update.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    serverCertificateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  UpdateServerCertificate
newUpdateServerCertificate pServerCertificateName_ =
  UpdateServerCertificate'
    { newPath' =
        Prelude.Nothing,
      newServerCertificateName' = Prelude.Nothing,
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
updateServerCertificate_newPath :: Lens.Lens' UpdateServerCertificate (Prelude.Maybe Prelude.Text)
updateServerCertificate_newPath = Lens.lens (\UpdateServerCertificate' {newPath'} -> newPath') (\s@UpdateServerCertificate' {} a -> s {newPath' = a} :: UpdateServerCertificate)

-- | The new name for the server certificate. Include this only if you are
-- updating the server certificate\'s name. The name of the certificate
-- cannot contain any spaces.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
updateServerCertificate_newServerCertificateName :: Lens.Lens' UpdateServerCertificate (Prelude.Maybe Prelude.Text)
updateServerCertificate_newServerCertificateName = Lens.lens (\UpdateServerCertificate' {newServerCertificateName'} -> newServerCertificateName') (\s@UpdateServerCertificate' {} a -> s {newServerCertificateName' = a} :: UpdateServerCertificate)

-- | The name of the server certificate that you want to update.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
updateServerCertificate_serverCertificateName :: Lens.Lens' UpdateServerCertificate Prelude.Text
updateServerCertificate_serverCertificateName = Lens.lens (\UpdateServerCertificate' {serverCertificateName} -> serverCertificateName) (\s@UpdateServerCertificate' {} a -> s {serverCertificateName = a} :: UpdateServerCertificate)

instance Prelude.AWSRequest UpdateServerCertificate where
  type
    Rs UpdateServerCertificate =
      UpdateServerCertificateResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      UpdateServerCertificateResponse'

instance Prelude.Hashable UpdateServerCertificate

instance Prelude.NFData UpdateServerCertificate

instance Prelude.ToHeaders UpdateServerCertificate where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath UpdateServerCertificate where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateServerCertificate where
  toQuery UpdateServerCertificate' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("UpdateServerCertificate" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-08" :: Prelude.ByteString),
        "NewPath" Prelude.=: newPath',
        "NewServerCertificateName"
          Prelude.=: newServerCertificateName',
        "ServerCertificateName"
          Prelude.=: serverCertificateName
      ]

-- | /See:/ 'newUpdateServerCertificateResponse' smart constructor.
data UpdateServerCertificateResponse = UpdateServerCertificateResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateServerCertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateServerCertificateResponse ::
  UpdateServerCertificateResponse
newUpdateServerCertificateResponse =
  UpdateServerCertificateResponse'

instance
  Prelude.NFData
    UpdateServerCertificateResponse
