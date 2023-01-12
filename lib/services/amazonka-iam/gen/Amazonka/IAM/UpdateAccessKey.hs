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
-- Module      : Amazonka.IAM.UpdateAccessKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the status of the specified access key from Active to Inactive,
-- or vice versa. This operation can be used to disable a user\'s key as
-- part of a key rotation workflow.
--
-- If the @UserName@ is not specified, the user name is determined
-- implicitly based on the Amazon Web Services access key ID used to sign
-- the request. If a temporary access key is used, then @UserName@ is
-- required. If a long-term key is assigned to the user, then @UserName@ is
-- not required. This operation works for access keys under the Amazon Web
-- Services account. Consequently, you can use this operation to manage
-- Amazon Web Services account root user credentials even if the Amazon Web
-- Services account has no associated users.
--
-- For information about rotating keys, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/ManagingCredentials.html Managing keys and certificates>
-- in the /IAM User Guide/.
module Amazonka.IAM.UpdateAccessKey
  ( -- * Creating a Request
    UpdateAccessKey (..),
    newUpdateAccessKey,

    -- * Request Lenses
    updateAccessKey_userName,
    updateAccessKey_accessKeyId,
    updateAccessKey_status,

    -- * Destructuring the Response
    UpdateAccessKeyResponse (..),
    newUpdateAccessKeyResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateAccessKey' smart constructor.
data UpdateAccessKey = UpdateAccessKey'
  { -- | The name of the user whose key you want to update.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    userName :: Prelude.Maybe Prelude.Text,
    -- | The access key ID of the secret access key you want to update.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- that can consist of any upper or lowercased letter or digit.
    accessKeyId :: Core.AccessKey,
    -- | The status you want to assign to the secret access key. @Active@ means
    -- that the key can be used for programmatic calls to Amazon Web Services,
    -- while @Inactive@ means that the key cannot be used.
    status :: StatusType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAccessKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userName', 'updateAccessKey_userName' - The name of the user whose key you want to update.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
--
-- 'accessKeyId', 'updateAccessKey_accessKeyId' - The access key ID of the secret access key you want to update.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that can consist of any upper or lowercased letter or digit.
--
-- 'status', 'updateAccessKey_status' - The status you want to assign to the secret access key. @Active@ means
-- that the key can be used for programmatic calls to Amazon Web Services,
-- while @Inactive@ means that the key cannot be used.
newUpdateAccessKey ::
  -- | 'accessKeyId'
  Core.AccessKey ->
  -- | 'status'
  StatusType ->
  UpdateAccessKey
newUpdateAccessKey pAccessKeyId_ pStatus_ =
  UpdateAccessKey'
    { userName = Prelude.Nothing,
      accessKeyId = pAccessKeyId_,
      status = pStatus_
    }

-- | The name of the user whose key you want to update.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
updateAccessKey_userName :: Lens.Lens' UpdateAccessKey (Prelude.Maybe Prelude.Text)
updateAccessKey_userName = Lens.lens (\UpdateAccessKey' {userName} -> userName) (\s@UpdateAccessKey' {} a -> s {userName = a} :: UpdateAccessKey)

-- | The access key ID of the secret access key you want to update.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that can consist of any upper or lowercased letter or digit.
updateAccessKey_accessKeyId :: Lens.Lens' UpdateAccessKey Core.AccessKey
updateAccessKey_accessKeyId = Lens.lens (\UpdateAccessKey' {accessKeyId} -> accessKeyId) (\s@UpdateAccessKey' {} a -> s {accessKeyId = a} :: UpdateAccessKey)

-- | The status you want to assign to the secret access key. @Active@ means
-- that the key can be used for programmatic calls to Amazon Web Services,
-- while @Inactive@ means that the key cannot be used.
updateAccessKey_status :: Lens.Lens' UpdateAccessKey StatusType
updateAccessKey_status = Lens.lens (\UpdateAccessKey' {status} -> status) (\s@UpdateAccessKey' {} a -> s {status = a} :: UpdateAccessKey)

instance Core.AWSRequest UpdateAccessKey where
  type
    AWSResponse UpdateAccessKey =
      UpdateAccessKeyResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull UpdateAccessKeyResponse'

instance Prelude.Hashable UpdateAccessKey where
  hashWithSalt _salt UpdateAccessKey' {..} =
    _salt `Prelude.hashWithSalt` userName
      `Prelude.hashWithSalt` accessKeyId
      `Prelude.hashWithSalt` status

instance Prelude.NFData UpdateAccessKey where
  rnf UpdateAccessKey' {..} =
    Prelude.rnf userName
      `Prelude.seq` Prelude.rnf accessKeyId
      `Prelude.seq` Prelude.rnf status

instance Data.ToHeaders UpdateAccessKey where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath UpdateAccessKey where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateAccessKey where
  toQuery UpdateAccessKey' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("UpdateAccessKey" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "UserName" Data.=: userName,
        "AccessKeyId" Data.=: accessKeyId,
        "Status" Data.=: status
      ]

-- | /See:/ 'newUpdateAccessKeyResponse' smart constructor.
data UpdateAccessKeyResponse = UpdateAccessKeyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAccessKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateAccessKeyResponse ::
  UpdateAccessKeyResponse
newUpdateAccessKeyResponse = UpdateAccessKeyResponse'

instance Prelude.NFData UpdateAccessKeyResponse where
  rnf _ = ()
