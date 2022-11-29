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
-- Module      : Amazonka.IAM.DeleteServiceSpecificCredential
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified service-specific credential.
module Amazonka.IAM.DeleteServiceSpecificCredential
  ( -- * Creating a Request
    DeleteServiceSpecificCredential (..),
    newDeleteServiceSpecificCredential,

    -- * Request Lenses
    deleteServiceSpecificCredential_userName,
    deleteServiceSpecificCredential_serviceSpecificCredentialId,

    -- * Destructuring the Response
    DeleteServiceSpecificCredentialResponse (..),
    newDeleteServiceSpecificCredentialResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteServiceSpecificCredential' smart constructor.
data DeleteServiceSpecificCredential = DeleteServiceSpecificCredential'
  { -- | The name of the IAM user associated with the service-specific
    -- credential. If this value is not specified, then the operation assumes
    -- the user whose credentials are used to call the operation.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    userName :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the service-specific credential. You can get
    -- this value by calling ListServiceSpecificCredentials.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- that can consist of any upper or lowercased letter or digit.
    serviceSpecificCredentialId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteServiceSpecificCredential' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userName', 'deleteServiceSpecificCredential_userName' - The name of the IAM user associated with the service-specific
-- credential. If this value is not specified, then the operation assumes
-- the user whose credentials are used to call the operation.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
--
-- 'serviceSpecificCredentialId', 'deleteServiceSpecificCredential_serviceSpecificCredentialId' - The unique identifier of the service-specific credential. You can get
-- this value by calling ListServiceSpecificCredentials.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that can consist of any upper or lowercased letter or digit.
newDeleteServiceSpecificCredential ::
  -- | 'serviceSpecificCredentialId'
  Prelude.Text ->
  DeleteServiceSpecificCredential
newDeleteServiceSpecificCredential
  pServiceSpecificCredentialId_ =
    DeleteServiceSpecificCredential'
      { userName =
          Prelude.Nothing,
        serviceSpecificCredentialId =
          pServiceSpecificCredentialId_
      }

-- | The name of the IAM user associated with the service-specific
-- credential. If this value is not specified, then the operation assumes
-- the user whose credentials are used to call the operation.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
deleteServiceSpecificCredential_userName :: Lens.Lens' DeleteServiceSpecificCredential (Prelude.Maybe Prelude.Text)
deleteServiceSpecificCredential_userName = Lens.lens (\DeleteServiceSpecificCredential' {userName} -> userName) (\s@DeleteServiceSpecificCredential' {} a -> s {userName = a} :: DeleteServiceSpecificCredential)

-- | The unique identifier of the service-specific credential. You can get
-- this value by calling ListServiceSpecificCredentials.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that can consist of any upper or lowercased letter or digit.
deleteServiceSpecificCredential_serviceSpecificCredentialId :: Lens.Lens' DeleteServiceSpecificCredential Prelude.Text
deleteServiceSpecificCredential_serviceSpecificCredentialId = Lens.lens (\DeleteServiceSpecificCredential' {serviceSpecificCredentialId} -> serviceSpecificCredentialId) (\s@DeleteServiceSpecificCredential' {} a -> s {serviceSpecificCredentialId = a} :: DeleteServiceSpecificCredential)

instance
  Core.AWSRequest
    DeleteServiceSpecificCredential
  where
  type
    AWSResponse DeleteServiceSpecificCredential =
      DeleteServiceSpecificCredentialResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull
      DeleteServiceSpecificCredentialResponse'

instance
  Prelude.Hashable
    DeleteServiceSpecificCredential
  where
  hashWithSalt
    _salt
    DeleteServiceSpecificCredential' {..} =
      _salt `Prelude.hashWithSalt` userName
        `Prelude.hashWithSalt` serviceSpecificCredentialId

instance
  Prelude.NFData
    DeleteServiceSpecificCredential
  where
  rnf DeleteServiceSpecificCredential' {..} =
    Prelude.rnf userName
      `Prelude.seq` Prelude.rnf serviceSpecificCredentialId

instance
  Core.ToHeaders
    DeleteServiceSpecificCredential
  where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteServiceSpecificCredential where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteServiceSpecificCredential where
  toQuery DeleteServiceSpecificCredential' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DeleteServiceSpecificCredential" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2010-05-08" :: Prelude.ByteString),
        "UserName" Core.=: userName,
        "ServiceSpecificCredentialId"
          Core.=: serviceSpecificCredentialId
      ]

-- | /See:/ 'newDeleteServiceSpecificCredentialResponse' smart constructor.
data DeleteServiceSpecificCredentialResponse = DeleteServiceSpecificCredentialResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteServiceSpecificCredentialResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteServiceSpecificCredentialResponse ::
  DeleteServiceSpecificCredentialResponse
newDeleteServiceSpecificCredentialResponse =
  DeleteServiceSpecificCredentialResponse'

instance
  Prelude.NFData
    DeleteServiceSpecificCredentialResponse
  where
  rnf _ = ()
