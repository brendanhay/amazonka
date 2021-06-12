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
-- Module      : Network.AWS.APIGateway.UpdateApiKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes information about an ApiKey resource.
module Network.AWS.APIGateway.UpdateApiKey
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
    apiKey_lastUpdatedDate,
    apiKey_stageKeys,
    apiKey_enabled,
    apiKey_id,
    apiKey_name,
    apiKey_tags,
    apiKey_description,
    apiKey_value,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to change information about an ApiKey resource.
--
-- /See:/ 'newUpdateApiKey' smart constructor.
data UpdateApiKey = UpdateApiKey'
  { -- | A list of update operations to be applied to the specified resource and
    -- in the order specified in this list.
    patchOperations :: Core.Maybe [PatchOperation],
    -- | [Required] The identifier of the ApiKey resource to be updated.
    apiKey :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  UpdateApiKey
newUpdateApiKey pApiKey_ =
  UpdateApiKey'
    { patchOperations = Core.Nothing,
      apiKey = pApiKey_
    }

-- | A list of update operations to be applied to the specified resource and
-- in the order specified in this list.
updateApiKey_patchOperations :: Lens.Lens' UpdateApiKey (Core.Maybe [PatchOperation])
updateApiKey_patchOperations = Lens.lens (\UpdateApiKey' {patchOperations} -> patchOperations) (\s@UpdateApiKey' {} a -> s {patchOperations = a} :: UpdateApiKey) Core.. Lens.mapping Lens._Coerce

-- | [Required] The identifier of the ApiKey resource to be updated.
updateApiKey_apiKey :: Lens.Lens' UpdateApiKey Core.Text
updateApiKey_apiKey = Lens.lens (\UpdateApiKey' {apiKey} -> apiKey) (\s@UpdateApiKey' {} a -> s {apiKey = a} :: UpdateApiKey)

instance Core.AWSRequest UpdateApiKey where
  type AWSResponse UpdateApiKey = ApiKey
  request = Request.patchJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Core.Hashable UpdateApiKey

instance Core.NFData UpdateApiKey

instance Core.ToHeaders UpdateApiKey where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateApiKey where
  toJSON UpdateApiKey' {..} =
    Core.object
      ( Core.catMaybes
          [ ("patchOperations" Core..=)
              Core.<$> patchOperations
          ]
      )

instance Core.ToPath UpdateApiKey where
  toPath UpdateApiKey' {..} =
    Core.mconcat ["/apikeys/", Core.toBS apiKey]

instance Core.ToQuery UpdateApiKey where
  toQuery = Core.const Core.mempty
