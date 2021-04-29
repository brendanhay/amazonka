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
-- Module      : Network.AWS.IAM.UpdateSigningCertificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the status of the specified user signing certificate from active
-- to disabled, or vice versa. This operation can be used to disable an IAM
-- user\'s signing certificate as part of a certificate rotation work flow.
--
-- If the @UserName@ field is not specified, the user name is determined
-- implicitly based on the AWS access key ID used to sign the request. This
-- operation works for access keys under the AWS account. Consequently, you
-- can use this operation to manage AWS account root user credentials even
-- if the AWS account has no associated users.
module Network.AWS.IAM.UpdateSigningCertificate
  ( -- * Creating a Request
    UpdateSigningCertificate (..),
    newUpdateSigningCertificate,

    -- * Request Lenses
    updateSigningCertificate_userName,
    updateSigningCertificate_certificateId,
    updateSigningCertificate_status,

    -- * Destructuring the Response
    UpdateSigningCertificateResponse (..),
    newUpdateSigningCertificateResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateSigningCertificate' smart constructor.
data UpdateSigningCertificate = UpdateSigningCertificate'
  { -- | The name of the IAM user the signing certificate belongs to.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    userName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the signing certificate you want to update.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- that can consist of any upper or lowercased letter or digit.
    certificateId :: Prelude.Text,
    -- | The status you want to assign to the certificate. @Active@ means that
    -- the certificate can be used for programmatic calls to AWS @Inactive@
    -- means that the certificate cannot be used.
    status :: StatusType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateSigningCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userName', 'updateSigningCertificate_userName' - The name of the IAM user the signing certificate belongs to.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
--
-- 'certificateId', 'updateSigningCertificate_certificateId' - The ID of the signing certificate you want to update.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that can consist of any upper or lowercased letter or digit.
--
-- 'status', 'updateSigningCertificate_status' - The status you want to assign to the certificate. @Active@ means that
-- the certificate can be used for programmatic calls to AWS @Inactive@
-- means that the certificate cannot be used.
newUpdateSigningCertificate ::
  -- | 'certificateId'
  Prelude.Text ->
  -- | 'status'
  StatusType ->
  UpdateSigningCertificate
newUpdateSigningCertificate pCertificateId_ pStatus_ =
  UpdateSigningCertificate'
    { userName =
        Prelude.Nothing,
      certificateId = pCertificateId_,
      status = pStatus_
    }

-- | The name of the IAM user the signing certificate belongs to.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
updateSigningCertificate_userName :: Lens.Lens' UpdateSigningCertificate (Prelude.Maybe Prelude.Text)
updateSigningCertificate_userName = Lens.lens (\UpdateSigningCertificate' {userName} -> userName) (\s@UpdateSigningCertificate' {} a -> s {userName = a} :: UpdateSigningCertificate)

-- | The ID of the signing certificate you want to update.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that can consist of any upper or lowercased letter or digit.
updateSigningCertificate_certificateId :: Lens.Lens' UpdateSigningCertificate Prelude.Text
updateSigningCertificate_certificateId = Lens.lens (\UpdateSigningCertificate' {certificateId} -> certificateId) (\s@UpdateSigningCertificate' {} a -> s {certificateId = a} :: UpdateSigningCertificate)

-- | The status you want to assign to the certificate. @Active@ means that
-- the certificate can be used for programmatic calls to AWS @Inactive@
-- means that the certificate cannot be used.
updateSigningCertificate_status :: Lens.Lens' UpdateSigningCertificate StatusType
updateSigningCertificate_status = Lens.lens (\UpdateSigningCertificate' {status} -> status) (\s@UpdateSigningCertificate' {} a -> s {status = a} :: UpdateSigningCertificate)

instance Prelude.AWSRequest UpdateSigningCertificate where
  type
    Rs UpdateSigningCertificate =
      UpdateSigningCertificateResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      UpdateSigningCertificateResponse'

instance Prelude.Hashable UpdateSigningCertificate

instance Prelude.NFData UpdateSigningCertificate

instance Prelude.ToHeaders UpdateSigningCertificate where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath UpdateSigningCertificate where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateSigningCertificate where
  toQuery UpdateSigningCertificate' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("UpdateSigningCertificate" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-08" :: Prelude.ByteString),
        "UserName" Prelude.=: userName,
        "CertificateId" Prelude.=: certificateId,
        "Status" Prelude.=: status
      ]

-- | /See:/ 'newUpdateSigningCertificateResponse' smart constructor.
data UpdateSigningCertificateResponse = UpdateSigningCertificateResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateSigningCertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateSigningCertificateResponse ::
  UpdateSigningCertificateResponse
newUpdateSigningCertificateResponse =
  UpdateSigningCertificateResponse'

instance
  Prelude.NFData
    UpdateSigningCertificateResponse
