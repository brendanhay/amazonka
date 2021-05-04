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
-- Module      : Network.AWS.IAM.DeleteServiceSpecificCredential
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified service-specific credential.
module Network.AWS.IAM.DeleteServiceSpecificCredential
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

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.AWSRequest
    DeleteServiceSpecificCredential
  where
  type
    Rs DeleteServiceSpecificCredential =
      DeleteServiceSpecificCredentialResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      DeleteServiceSpecificCredentialResponse'

instance
  Prelude.Hashable
    DeleteServiceSpecificCredential

instance
  Prelude.NFData
    DeleteServiceSpecificCredential

instance
  Prelude.ToHeaders
    DeleteServiceSpecificCredential
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToPath
    DeleteServiceSpecificCredential
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DeleteServiceSpecificCredential
  where
  toQuery DeleteServiceSpecificCredential' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "DeleteServiceSpecificCredential" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2010-05-08" :: Prelude.ByteString),
        "UserName" Prelude.=: userName,
        "ServiceSpecificCredentialId"
          Prelude.=: serviceSpecificCredentialId
      ]

-- | /See:/ 'newDeleteServiceSpecificCredentialResponse' smart constructor.
data DeleteServiceSpecificCredentialResponse = DeleteServiceSpecificCredentialResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
