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
-- Module      : Network.AWS.IAM.CreateAccountAlias
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an alias for your AWS account. For information about using an
-- AWS account alias, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/AccountAlias.html Using an alias for your AWS account ID>
-- in the /IAM User Guide/.
module Network.AWS.IAM.CreateAccountAlias
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

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest CreateAccountAlias where
  type
    Rs CreateAccountAlias =
      CreateAccountAliasResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull CreateAccountAliasResponse'

instance Prelude.Hashable CreateAccountAlias

instance Prelude.NFData CreateAccountAlias

instance Prelude.ToHeaders CreateAccountAlias where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath CreateAccountAlias where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateAccountAlias where
  toQuery CreateAccountAlias' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("CreateAccountAlias" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-08" :: Prelude.ByteString),
        "AccountAlias" Prelude.=: accountAlias
      ]

-- | /See:/ 'newCreateAccountAliasResponse' smart constructor.
data CreateAccountAliasResponse = CreateAccountAliasResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateAccountAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newCreateAccountAliasResponse ::
  CreateAccountAliasResponse
newCreateAccountAliasResponse =
  CreateAccountAliasResponse'

instance Prelude.NFData CreateAccountAliasResponse
