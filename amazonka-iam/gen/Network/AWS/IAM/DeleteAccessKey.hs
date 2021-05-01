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
-- Module      : Network.AWS.IAM.DeleteAccessKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the access key pair associated with the specified IAM user.
--
-- If you do not specify a user name, IAM determines the user name
-- implicitly based on the AWS access key ID signing the request. This
-- operation works for access keys under the AWS account. Consequently, you
-- can use this operation to manage AWS account root user credentials even
-- if the AWS account has no associated users.
module Network.AWS.IAM.DeleteAccessKey
  ( -- * Creating a Request
    DeleteAccessKey (..),
    newDeleteAccessKey,

    -- * Request Lenses
    deleteAccessKey_userName,
    deleteAccessKey_accessKeyId,

    -- * Destructuring the Response
    DeleteAccessKeyResponse (..),
    newDeleteAccessKeyResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteAccessKey' smart constructor.
data DeleteAccessKey = DeleteAccessKey'
  { -- | The name of the user whose access key pair you want to delete.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    userName :: Prelude.Maybe Prelude.Text,
    -- | The access key ID for the access key ID and secret access key you want
    -- to delete.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- that can consist of any upper or lowercased letter or digit.
    accessKeyId :: Prelude.AccessKey
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteAccessKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userName', 'deleteAccessKey_userName' - The name of the user whose access key pair you want to delete.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
--
-- 'accessKeyId', 'deleteAccessKey_accessKeyId' - The access key ID for the access key ID and secret access key you want
-- to delete.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that can consist of any upper or lowercased letter or digit.
newDeleteAccessKey ::
  -- | 'accessKeyId'
  Prelude.AccessKey ->
  DeleteAccessKey
newDeleteAccessKey pAccessKeyId_ =
  DeleteAccessKey'
    { userName = Prelude.Nothing,
      accessKeyId = pAccessKeyId_
    }

-- | The name of the user whose access key pair you want to delete.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
deleteAccessKey_userName :: Lens.Lens' DeleteAccessKey (Prelude.Maybe Prelude.Text)
deleteAccessKey_userName = Lens.lens (\DeleteAccessKey' {userName} -> userName) (\s@DeleteAccessKey' {} a -> s {userName = a} :: DeleteAccessKey)

-- | The access key ID for the access key ID and secret access key you want
-- to delete.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that can consist of any upper or lowercased letter or digit.
deleteAccessKey_accessKeyId :: Lens.Lens' DeleteAccessKey Prelude.AccessKey
deleteAccessKey_accessKeyId = Lens.lens (\DeleteAccessKey' {accessKeyId} -> accessKeyId) (\s@DeleteAccessKey' {} a -> s {accessKeyId = a} :: DeleteAccessKey)

instance Prelude.AWSRequest DeleteAccessKey where
  type Rs DeleteAccessKey = DeleteAccessKeyResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull DeleteAccessKeyResponse'

instance Prelude.Hashable DeleteAccessKey

instance Prelude.NFData DeleteAccessKey

instance Prelude.ToHeaders DeleteAccessKey where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteAccessKey where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteAccessKey where
  toQuery DeleteAccessKey' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DeleteAccessKey" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-08" :: Prelude.ByteString),
        "UserName" Prelude.=: userName,
        "AccessKeyId" Prelude.=: accessKeyId
      ]

-- | /See:/ 'newDeleteAccessKeyResponse' smart constructor.
data DeleteAccessKeyResponse = DeleteAccessKeyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteAccessKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteAccessKeyResponse ::
  DeleteAccessKeyResponse
newDeleteAccessKeyResponse = DeleteAccessKeyResponse'

instance Prelude.NFData DeleteAccessKeyResponse
