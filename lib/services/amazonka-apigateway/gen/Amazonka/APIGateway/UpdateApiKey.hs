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
-- Module      : Amazonka.APIGateway.UpdateApiKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes information about an ApiKey resource.
module Amazonka.APIGateway.UpdateApiKey
  ( -- * Creating a Request
    UpdateApiKey (..),
    newUpdateApiKey,

    -- * Request Lenses
    updateApiKey_patchOperations,
    updateApiKey_apiKey,

    -- * Destructuring the Response
    ApiKey (..),
    newApiKey,

    -- * Response Lenses
    apiKey_tags,
    apiKey_customerId,
    apiKey_name,
    apiKey_lastUpdatedDate,
    apiKey_description,
    apiKey_stageKeys,
    apiKey_id,
    apiKey_enabled,
    apiKey_createdDate,
    apiKey_value,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A request to change information about an ApiKey resource.
--
-- /See:/ 'newUpdateApiKey' smart constructor.
data UpdateApiKey = UpdateApiKey'
  { -- | A list of update operations to be applied to the specified resource and
    -- in the order specified in this list.
    patchOperations :: Prelude.Maybe [PatchOperation],
    -- | [Required] The identifier of the ApiKey resource to be updated.
    apiKey :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateApiKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'patchOperations', 'updateApiKey_patchOperations' - A list of update operations to be applied to the specified resource and
-- in the order specified in this list.
--
-- 'apiKey', 'updateApiKey_apiKey' - [Required] The identifier of the ApiKey resource to be updated.
newUpdateApiKey ::
  -- | 'apiKey'
  Prelude.Text ->
  UpdateApiKey
newUpdateApiKey pApiKey_ =
  UpdateApiKey'
    { patchOperations = Prelude.Nothing,
      apiKey = pApiKey_
    }

-- | A list of update operations to be applied to the specified resource and
-- in the order specified in this list.
updateApiKey_patchOperations :: Lens.Lens' UpdateApiKey (Prelude.Maybe [PatchOperation])
updateApiKey_patchOperations = Lens.lens (\UpdateApiKey' {patchOperations} -> patchOperations) (\s@UpdateApiKey' {} a -> s {patchOperations = a} :: UpdateApiKey) Prelude.. Lens.mapping Lens.coerced

-- | [Required] The identifier of the ApiKey resource to be updated.
updateApiKey_apiKey :: Lens.Lens' UpdateApiKey Prelude.Text
updateApiKey_apiKey = Lens.lens (\UpdateApiKey' {apiKey} -> apiKey) (\s@UpdateApiKey' {} a -> s {apiKey = a} :: UpdateApiKey)

instance Core.AWSRequest UpdateApiKey where
  type AWSResponse UpdateApiKey = ApiKey
  request = Request.patchJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable UpdateApiKey where
  hashWithSalt _salt UpdateApiKey' {..} =
    _salt `Prelude.hashWithSalt` patchOperations
      `Prelude.hashWithSalt` apiKey

instance Prelude.NFData UpdateApiKey where
  rnf UpdateApiKey' {..} =
    Prelude.rnf patchOperations
      `Prelude.seq` Prelude.rnf apiKey

instance Core.ToHeaders UpdateApiKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Core.ToJSON UpdateApiKey where
  toJSON UpdateApiKey' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("patchOperations" Core..=)
              Prelude.<$> patchOperations
          ]
      )

instance Core.ToPath UpdateApiKey where
  toPath UpdateApiKey' {..} =
    Prelude.mconcat ["/apikeys/", Core.toBS apiKey]

instance Core.ToQuery UpdateApiKey where
  toQuery = Prelude.const Prelude.mempty
