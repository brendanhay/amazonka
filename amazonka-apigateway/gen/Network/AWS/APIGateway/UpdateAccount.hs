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
-- Module      : Network.AWS.APIGateway.UpdateAccount
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes information about the current Account resource.
module Network.AWS.APIGateway.UpdateAccount
  ( -- * Creating a Request
    UpdateAccount (..),
    newUpdateAccount,

    -- * Request Lenses
    updateAccount_patchOperations,

    -- * Destructuring the Response
    Account (..),
    newAccount,

    -- * Response Lenses
    account_throttleSettings,
    account_apiKeyVersion,
    account_features,
    account_cloudwatchRoleArn,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Requests API Gateway to change information about the current Account
-- resource.
--
-- /See:/ 'newUpdateAccount' smart constructor.
data UpdateAccount = UpdateAccount'
  { -- | A list of update operations to be applied to the specified resource and
    -- in the order specified in this list.
    patchOperations :: Prelude.Maybe [PatchOperation]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'patchOperations', 'updateAccount_patchOperations' - A list of update operations to be applied to the specified resource and
-- in the order specified in this list.
newUpdateAccount ::
  UpdateAccount
newUpdateAccount =
  UpdateAccount' {patchOperations = Prelude.Nothing}

-- | A list of update operations to be applied to the specified resource and
-- in the order specified in this list.
updateAccount_patchOperations :: Lens.Lens' UpdateAccount (Prelude.Maybe [PatchOperation])
updateAccount_patchOperations = Lens.lens (\UpdateAccount' {patchOperations} -> patchOperations) (\s@UpdateAccount' {} a -> s {patchOperations = a} :: UpdateAccount) Prelude.. Lens.mapping Lens._Coerce

instance Core.AWSRequest UpdateAccount where
  type AWSResponse UpdateAccount = Account
  request = Request.patchJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable UpdateAccount

instance Prelude.NFData UpdateAccount

instance Core.ToHeaders UpdateAccount where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Core.ToJSON UpdateAccount where
  toJSON UpdateAccount' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("patchOperations" Core..=)
              Prelude.<$> patchOperations
          ]
      )

instance Core.ToPath UpdateAccount where
  toPath = Prelude.const "/account"

instance Core.ToQuery UpdateAccount where
  toQuery = Prelude.const Prelude.mempty
