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
-- Module      : Network.AWS.WorkDocs.DeactivateUser
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deactivates the specified user, which revokes the user\'s access to
-- Amazon WorkDocs.
module Network.AWS.WorkDocs.DeactivateUser
  ( -- * Creating a Request
    DeactivateUser (..),
    newDeactivateUser,

    -- * Request Lenses
    deactivateUser_authenticationToken,
    deactivateUser_userId,

    -- * Destructuring the Response
    DeactivateUserResponse (..),
    newDeactivateUserResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkDocs.Types

-- | /See:/ 'newDeactivateUser' smart constructor.
data DeactivateUser = DeactivateUser'
  { -- | Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The ID of the user.
    userId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeactivateUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authenticationToken', 'deactivateUser_authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
--
-- 'userId', 'deactivateUser_userId' - The ID of the user.
newDeactivateUser ::
  -- | 'userId'
  Prelude.Text ->
  DeactivateUser
newDeactivateUser pUserId_ =
  DeactivateUser'
    { authenticationToken =
        Prelude.Nothing,
      userId = pUserId_
    }

-- | Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
deactivateUser_authenticationToken :: Lens.Lens' DeactivateUser (Prelude.Maybe Prelude.Text)
deactivateUser_authenticationToken = Lens.lens (\DeactivateUser' {authenticationToken} -> authenticationToken) (\s@DeactivateUser' {} a -> s {authenticationToken = a} :: DeactivateUser) Prelude.. Lens.mapping Prelude._Sensitive

-- | The ID of the user.
deactivateUser_userId :: Lens.Lens' DeactivateUser Prelude.Text
deactivateUser_userId = Lens.lens (\DeactivateUser' {userId} -> userId) (\s@DeactivateUser' {} a -> s {userId = a} :: DeactivateUser)

instance Prelude.AWSRequest DeactivateUser where
  type Rs DeactivateUser = DeactivateUserResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull DeactivateUserResponse'

instance Prelude.Hashable DeactivateUser

instance Prelude.NFData DeactivateUser

instance Prelude.ToHeaders DeactivateUser where
  toHeaders DeactivateUser' {..} =
    Prelude.mconcat
      [ "Authentication" Prelude.=# authenticationToken,
        "Content-Type"
          Prelude.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Prelude.ToPath DeactivateUser where
  toPath DeactivateUser' {..} =
    Prelude.mconcat
      [ "/api/v1/users/",
        Prelude.toBS userId,
        "/activation"
      ]

instance Prelude.ToQuery DeactivateUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeactivateUserResponse' smart constructor.
data DeactivateUserResponse = DeactivateUserResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeactivateUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeactivateUserResponse ::
  DeactivateUserResponse
newDeactivateUserResponse = DeactivateUserResponse'

instance Prelude.NFData DeactivateUserResponse
