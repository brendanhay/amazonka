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
-- Module      : Amazonka.SupportApp.PutAccountAlias
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates an individual alias for each Amazon Web Services
-- account ID. The alias appears in the Amazon Web Services Support App
-- page of the Amazon Web Services Support Center. The alias also appears
-- in Slack messages from the Amazon Web Services Support App.
module Amazonka.SupportApp.PutAccountAlias
  ( -- * Creating a Request
    PutAccountAlias (..),
    newPutAccountAlias,

    -- * Request Lenses
    putAccountAlias_accountAlias,

    -- * Destructuring the Response
    PutAccountAliasResponse (..),
    newPutAccountAliasResponse,

    -- * Response Lenses
    putAccountAliasResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SupportApp.Types

-- | /See:/ 'newPutAccountAlias' smart constructor.
data PutAccountAlias = PutAccountAlias'
  { -- | An alias or short name for an Amazon Web Services account.
    accountAlias :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutAccountAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountAlias', 'putAccountAlias_accountAlias' - An alias or short name for an Amazon Web Services account.
newPutAccountAlias ::
  -- | 'accountAlias'
  Prelude.Text ->
  PutAccountAlias
newPutAccountAlias pAccountAlias_ =
  PutAccountAlias' {accountAlias = pAccountAlias_}

-- | An alias or short name for an Amazon Web Services account.
putAccountAlias_accountAlias :: Lens.Lens' PutAccountAlias Prelude.Text
putAccountAlias_accountAlias = Lens.lens (\PutAccountAlias' {accountAlias} -> accountAlias) (\s@PutAccountAlias' {} a -> s {accountAlias = a} :: PutAccountAlias)

instance Core.AWSRequest PutAccountAlias where
  type
    AWSResponse PutAccountAlias =
      PutAccountAliasResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutAccountAliasResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutAccountAlias where
  hashWithSalt _salt PutAccountAlias' {..} =
    _salt `Prelude.hashWithSalt` accountAlias

instance Prelude.NFData PutAccountAlias where
  rnf PutAccountAlias' {..} = Prelude.rnf accountAlias

instance Data.ToHeaders PutAccountAlias where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutAccountAlias where
  toJSON PutAccountAlias' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("accountAlias" Data..= accountAlias)]
      )

instance Data.ToPath PutAccountAlias where
  toPath = Prelude.const "/control/put-account-alias"

instance Data.ToQuery PutAccountAlias where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutAccountAliasResponse' smart constructor.
data PutAccountAliasResponse = PutAccountAliasResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutAccountAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putAccountAliasResponse_httpStatus' - The response's http status code.
newPutAccountAliasResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutAccountAliasResponse
newPutAccountAliasResponse pHttpStatus_ =
  PutAccountAliasResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
putAccountAliasResponse_httpStatus :: Lens.Lens' PutAccountAliasResponse Prelude.Int
putAccountAliasResponse_httpStatus = Lens.lens (\PutAccountAliasResponse' {httpStatus} -> httpStatus) (\s@PutAccountAliasResponse' {} a -> s {httpStatus = a} :: PutAccountAliasResponse)

instance Prelude.NFData PutAccountAliasResponse where
  rnf PutAccountAliasResponse' {..} =
    Prelude.rnf httpStatus
