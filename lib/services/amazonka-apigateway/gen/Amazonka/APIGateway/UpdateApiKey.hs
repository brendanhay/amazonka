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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
    apiKey_createdDate,
    apiKey_customerId,
    apiKey_description,
    apiKey_enabled,
    apiKey_id,
    apiKey_lastUpdatedDate,
    apiKey_name,
    apiKey_stageKeys,
    apiKey_tags,
    apiKey_value,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A request to change information about an ApiKey resource.
--
-- /See:/ 'newUpdateApiKey' smart constructor.
data UpdateApiKey = UpdateApiKey'
  { -- | For more information about supported patch operations, see
    -- <https://docs.aws.amazon.com/apigateway/latest/api/patch-operations.html Patch Operations>.
    patchOperations :: Prelude.Maybe [PatchOperation],
    -- | The identifier of the ApiKey resource to be updated.
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
-- 'patchOperations', 'updateApiKey_patchOperations' - For more information about supported patch operations, see
-- <https://docs.aws.amazon.com/apigateway/latest/api/patch-operations.html Patch Operations>.
--
-- 'apiKey', 'updateApiKey_apiKey' - The identifier of the ApiKey resource to be updated.
newUpdateApiKey ::
  -- | 'apiKey'
  Prelude.Text ->
  UpdateApiKey
newUpdateApiKey pApiKey_ =
  UpdateApiKey'
    { patchOperations = Prelude.Nothing,
      apiKey = pApiKey_
    }

-- | For more information about supported patch operations, see
-- <https://docs.aws.amazon.com/apigateway/latest/api/patch-operations.html Patch Operations>.
updateApiKey_patchOperations :: Lens.Lens' UpdateApiKey (Prelude.Maybe [PatchOperation])
updateApiKey_patchOperations = Lens.lens (\UpdateApiKey' {patchOperations} -> patchOperations) (\s@UpdateApiKey' {} a -> s {patchOperations = a} :: UpdateApiKey) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the ApiKey resource to be updated.
updateApiKey_apiKey :: Lens.Lens' UpdateApiKey Prelude.Text
updateApiKey_apiKey = Lens.lens (\UpdateApiKey' {apiKey} -> apiKey) (\s@UpdateApiKey' {} a -> s {apiKey = a} :: UpdateApiKey)

instance Core.AWSRequest UpdateApiKey where
  type AWSResponse UpdateApiKey = ApiKey
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable UpdateApiKey where
  hashWithSalt _salt UpdateApiKey' {..} =
    _salt
      `Prelude.hashWithSalt` patchOperations
      `Prelude.hashWithSalt` apiKey

instance Prelude.NFData UpdateApiKey where
  rnf UpdateApiKey' {..} =
    Prelude.rnf patchOperations `Prelude.seq`
      Prelude.rnf apiKey

instance Data.ToHeaders UpdateApiKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToJSON UpdateApiKey where
  toJSON UpdateApiKey' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("patchOperations" Data..=)
              Prelude.<$> patchOperations
          ]
      )

instance Data.ToPath UpdateApiKey where
  toPath UpdateApiKey' {..} =
    Prelude.mconcat ["/apikeys/", Data.toBS apiKey]

instance Data.ToQuery UpdateApiKey where
  toQuery = Prelude.const Prelude.mempty
