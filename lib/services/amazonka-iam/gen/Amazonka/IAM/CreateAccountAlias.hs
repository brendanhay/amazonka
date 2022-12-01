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
-- Module      : Amazonka.IAM.CreateAccountAlias
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an alias for your Amazon Web Services account. For information
-- about using an Amazon Web Services account alias, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/AccountAlias.html Using an alias for your Amazon Web Services account ID>
-- in the /IAM User Guide/.
module Amazonka.IAM.CreateAccountAlias
  ( -- * Creating a Request
    CreateAccountAlias (..),
    newCreateAccountAlias,

    -- * Request Lenses
    createAccountAlias_accountAlias,

    -- * Destructuring the Response
    CreateAccountAliasResponse (..),
    newCreateAccountAliasResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateAccountAlias' smart constructor.
data CreateAccountAlias = CreateAccountAlias'
  { -- | The account alias to create.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of lowercase letters, digits, and dashes. You cannot start or
    -- finish with a dash, nor can you have two dashes in a row.
    accountAlias :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAccountAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountAlias', 'createAccountAlias_accountAlias' - The account alias to create.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of lowercase letters, digits, and dashes. You cannot start or
-- finish with a dash, nor can you have two dashes in a row.
newCreateAccountAlias ::
  -- | 'accountAlias'
  Prelude.Text ->
  CreateAccountAlias
newCreateAccountAlias pAccountAlias_ =
  CreateAccountAlias' {accountAlias = pAccountAlias_}

-- | The account alias to create.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of lowercase letters, digits, and dashes. You cannot start or
-- finish with a dash, nor can you have two dashes in a row.
createAccountAlias_accountAlias :: Lens.Lens' CreateAccountAlias Prelude.Text
createAccountAlias_accountAlias = Lens.lens (\CreateAccountAlias' {accountAlias} -> accountAlias) (\s@CreateAccountAlias' {} a -> s {accountAlias = a} :: CreateAccountAlias)

instance Core.AWSRequest CreateAccountAlias where
  type
    AWSResponse CreateAccountAlias =
      CreateAccountAliasResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull CreateAccountAliasResponse'

instance Prelude.Hashable CreateAccountAlias where
  hashWithSalt _salt CreateAccountAlias' {..} =
    _salt `Prelude.hashWithSalt` accountAlias

instance Prelude.NFData CreateAccountAlias where
  rnf CreateAccountAlias' {..} =
    Prelude.rnf accountAlias

instance Core.ToHeaders CreateAccountAlias where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateAccountAlias where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateAccountAlias where
  toQuery CreateAccountAlias' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("CreateAccountAlias" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-05-08" :: Prelude.ByteString),
        "AccountAlias" Core.=: accountAlias
      ]

-- | /See:/ 'newCreateAccountAliasResponse' smart constructor.
data CreateAccountAliasResponse = CreateAccountAliasResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAccountAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newCreateAccountAliasResponse ::
  CreateAccountAliasResponse
newCreateAccountAliasResponse =
  CreateAccountAliasResponse'

instance Prelude.NFData CreateAccountAliasResponse where
  rnf _ = ()
