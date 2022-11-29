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
-- Module      : Amazonka.IAM.DeleteSigningCertificate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a signing certificate associated with the specified IAM user.
--
-- If you do not specify a user name, IAM determines the user name
-- implicitly based on the Amazon Web Services access key ID signing the
-- request. This operation works for access keys under the Amazon Web
-- Services account. Consequently, you can use this operation to manage
-- Amazon Web Services account root user credentials even if the Amazon Web
-- Services account has no associated IAM users.
module Amazonka.IAM.DeleteSigningCertificate
  ( -- * Creating a Request
    DeleteSigningCertificate (..),
    newDeleteSigningCertificate,

    -- * Request Lenses
    deleteSigningCertificate_userName,
    deleteSigningCertificate_certificateId,

    -- * Destructuring the Response
    DeleteSigningCertificateResponse (..),
    newDeleteSigningCertificateResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteSigningCertificate' smart constructor.
data DeleteSigningCertificate = DeleteSigningCertificate'
  { -- | The name of the user the signing certificate belongs to.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    userName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the signing certificate to delete.
    --
    -- The format of this parameter, as described by its
    -- <http://wikipedia.org/wiki/regex regex> pattern, is a string of
    -- characters that can be upper- or lower-cased letters or digits.
    certificateId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSigningCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userName', 'deleteSigningCertificate_userName' - The name of the user the signing certificate belongs to.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
--
-- 'certificateId', 'deleteSigningCertificate_certificateId' - The ID of the signing certificate to delete.
--
-- The format of this parameter, as described by its
-- <http://wikipedia.org/wiki/regex regex> pattern, is a string of
-- characters that can be upper- or lower-cased letters or digits.
newDeleteSigningCertificate ::
  -- | 'certificateId'
  Prelude.Text ->
  DeleteSigningCertificate
newDeleteSigningCertificate pCertificateId_ =
  DeleteSigningCertificate'
    { userName =
        Prelude.Nothing,
      certificateId = pCertificateId_
    }

-- | The name of the user the signing certificate belongs to.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
deleteSigningCertificate_userName :: Lens.Lens' DeleteSigningCertificate (Prelude.Maybe Prelude.Text)
deleteSigningCertificate_userName = Lens.lens (\DeleteSigningCertificate' {userName} -> userName) (\s@DeleteSigningCertificate' {} a -> s {userName = a} :: DeleteSigningCertificate)

-- | The ID of the signing certificate to delete.
--
-- The format of this parameter, as described by its
-- <http://wikipedia.org/wiki/regex regex> pattern, is a string of
-- characters that can be upper- or lower-cased letters or digits.
deleteSigningCertificate_certificateId :: Lens.Lens' DeleteSigningCertificate Prelude.Text
deleteSigningCertificate_certificateId = Lens.lens (\DeleteSigningCertificate' {certificateId} -> certificateId) (\s@DeleteSigningCertificate' {} a -> s {certificateId = a} :: DeleteSigningCertificate)

instance Core.AWSRequest DeleteSigningCertificate where
  type
    AWSResponse DeleteSigningCertificate =
      DeleteSigningCertificateResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull
      DeleteSigningCertificateResponse'

instance Prelude.Hashable DeleteSigningCertificate where
  hashWithSalt _salt DeleteSigningCertificate' {..} =
    _salt `Prelude.hashWithSalt` userName
      `Prelude.hashWithSalt` certificateId

instance Prelude.NFData DeleteSigningCertificate where
  rnf DeleteSigningCertificate' {..} =
    Prelude.rnf userName
      `Prelude.seq` Prelude.rnf certificateId

instance Core.ToHeaders DeleteSigningCertificate where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteSigningCertificate where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteSigningCertificate where
  toQuery DeleteSigningCertificate' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DeleteSigningCertificate" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-05-08" :: Prelude.ByteString),
        "UserName" Core.=: userName,
        "CertificateId" Core.=: certificateId
      ]

-- | /See:/ 'newDeleteSigningCertificateResponse' smart constructor.
data DeleteSigningCertificateResponse = DeleteSigningCertificateResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSigningCertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteSigningCertificateResponse ::
  DeleteSigningCertificateResponse
newDeleteSigningCertificateResponse =
  DeleteSigningCertificateResponse'

instance
  Prelude.NFData
    DeleteSigningCertificateResponse
  where
  rnf _ = ()
