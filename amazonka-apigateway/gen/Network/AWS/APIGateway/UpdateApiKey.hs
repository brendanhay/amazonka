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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
updateApiKey_patchOperations = Lens.lens (\UpdateApiKey' {patchOperations} -> patchOperations) (\s@UpdateApiKey' {} a -> s {patchOperations = a} :: UpdateApiKey) Prelude.. Lens.mapping Prelude._Coerce

-- | [Required] The identifier of the ApiKey resource to be updated.
updateApiKey_apiKey :: Lens.Lens' UpdateApiKey Prelude.Text
updateApiKey_apiKey = Lens.lens (\UpdateApiKey' {apiKey} -> apiKey) (\s@UpdateApiKey' {} a -> s {apiKey = a} :: UpdateApiKey)

instance Prelude.AWSRequest UpdateApiKey where
  type Rs UpdateApiKey = ApiKey
  request = Request.patchJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Prelude.eitherParseJSON x)

instance Prelude.Hashable UpdateApiKey

instance Prelude.NFData UpdateApiKey

instance Prelude.ToHeaders UpdateApiKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Prelude.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Prelude.ToJSON UpdateApiKey where
  toJSON UpdateApiKey' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("patchOperations" Prelude..=)
              Prelude.<$> patchOperations
          ]
      )

instance Prelude.ToPath UpdateApiKey where
  toPath UpdateApiKey' {..} =
    Prelude.mconcat ["/apikeys/", Prelude.toBS apiKey]

instance Prelude.ToQuery UpdateApiKey where
  toQuery = Prelude.const Prelude.mempty
