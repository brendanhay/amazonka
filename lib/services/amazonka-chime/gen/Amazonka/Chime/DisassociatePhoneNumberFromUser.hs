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
-- Module      : Amazonka.Chime.DisassociatePhoneNumberFromUser
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the primary provisioned phone number from the specified
-- Amazon Chime user.
module Amazonka.Chime.DisassociatePhoneNumberFromUser
  ( -- * Creating a Request
    DisassociatePhoneNumberFromUser (..),
    newDisassociatePhoneNumberFromUser,

    -- * Request Lenses
    disassociatePhoneNumberFromUser_accountId,
    disassociatePhoneNumberFromUser_userId,

    -- * Destructuring the Response
    DisassociatePhoneNumberFromUserResponse (..),
    newDisassociatePhoneNumberFromUserResponse,

    -- * Response Lenses
    disassociatePhoneNumberFromUserResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociatePhoneNumberFromUser' smart constructor.
data DisassociatePhoneNumberFromUser = DisassociatePhoneNumberFromUser'
  { -- | The Amazon Chime account ID.
    accountId :: Prelude.Text,
    -- | The user ID.
    userId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociatePhoneNumberFromUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'disassociatePhoneNumberFromUser_accountId' - The Amazon Chime account ID.
--
-- 'userId', 'disassociatePhoneNumberFromUser_userId' - The user ID.
newDisassociatePhoneNumberFromUser ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'userId'
  Prelude.Text ->
  DisassociatePhoneNumberFromUser
newDisassociatePhoneNumberFromUser
  pAccountId_
  pUserId_ =
    DisassociatePhoneNumberFromUser'
      { accountId =
          pAccountId_,
        userId = pUserId_
      }

-- | The Amazon Chime account ID.
disassociatePhoneNumberFromUser_accountId :: Lens.Lens' DisassociatePhoneNumberFromUser Prelude.Text
disassociatePhoneNumberFromUser_accountId = Lens.lens (\DisassociatePhoneNumberFromUser' {accountId} -> accountId) (\s@DisassociatePhoneNumberFromUser' {} a -> s {accountId = a} :: DisassociatePhoneNumberFromUser)

-- | The user ID.
disassociatePhoneNumberFromUser_userId :: Lens.Lens' DisassociatePhoneNumberFromUser Prelude.Text
disassociatePhoneNumberFromUser_userId = Lens.lens (\DisassociatePhoneNumberFromUser' {userId} -> userId) (\s@DisassociatePhoneNumberFromUser' {} a -> s {userId = a} :: DisassociatePhoneNumberFromUser)

instance
  Core.AWSRequest
    DisassociatePhoneNumberFromUser
  where
  type
    AWSResponse DisassociatePhoneNumberFromUser =
      DisassociatePhoneNumberFromUserResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociatePhoneNumberFromUserResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociatePhoneNumberFromUser
  where
  hashWithSalt
    _salt
    DisassociatePhoneNumberFromUser' {..} =
      _salt `Prelude.hashWithSalt` accountId
        `Prelude.hashWithSalt` userId

instance
  Prelude.NFData
    DisassociatePhoneNumberFromUser
  where
  rnf DisassociatePhoneNumberFromUser' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf userId

instance
  Data.ToHeaders
    DisassociatePhoneNumberFromUser
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON DisassociatePhoneNumberFromUser where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath DisassociatePhoneNumberFromUser where
  toPath DisassociatePhoneNumberFromUser' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS accountId,
        "/users/",
        Data.toBS userId
      ]

instance Data.ToQuery DisassociatePhoneNumberFromUser where
  toQuery =
    Prelude.const
      ( Prelude.mconcat
          ["operation=disassociate-phone-number"]
      )

-- | /See:/ 'newDisassociatePhoneNumberFromUserResponse' smart constructor.
data DisassociatePhoneNumberFromUserResponse = DisassociatePhoneNumberFromUserResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociatePhoneNumberFromUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociatePhoneNumberFromUserResponse_httpStatus' - The response's http status code.
newDisassociatePhoneNumberFromUserResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociatePhoneNumberFromUserResponse
newDisassociatePhoneNumberFromUserResponse
  pHttpStatus_ =
    DisassociatePhoneNumberFromUserResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
disassociatePhoneNumberFromUserResponse_httpStatus :: Lens.Lens' DisassociatePhoneNumberFromUserResponse Prelude.Int
disassociatePhoneNumberFromUserResponse_httpStatus = Lens.lens (\DisassociatePhoneNumberFromUserResponse' {httpStatus} -> httpStatus) (\s@DisassociatePhoneNumberFromUserResponse' {} a -> s {httpStatus = a} :: DisassociatePhoneNumberFromUserResponse)

instance
  Prelude.NFData
    DisassociatePhoneNumberFromUserResponse
  where
  rnf DisassociatePhoneNumberFromUserResponse' {..} =
    Prelude.rnf httpStatus
