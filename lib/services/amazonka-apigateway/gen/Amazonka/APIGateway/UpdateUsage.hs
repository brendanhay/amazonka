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
-- Module      : Amazonka.APIGateway.UpdateUsage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Grants a temporary extension to the remaining quota of a usage plan
-- associated with a specified API key.
module Amazonka.APIGateway.UpdateUsage
  ( -- * Creating a Request
    UpdateUsage (..),
    newUpdateUsage,

    -- * Request Lenses
    updateUsage_patchOperations,
    updateUsage_usagePlanId,
    updateUsage_keyId,

    -- * Destructuring the Response
    Usage (..),
    newUsage,

    -- * Response Lenses
    usage_endDate,
    usage_items,
    usage_position,
    usage_startDate,
    usage_usagePlanId,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The PATCH request to grant a temporary extension to the remaining quota
-- of a usage plan associated with a specified API key.
--
-- /See:/ 'newUpdateUsage' smart constructor.
data UpdateUsage = UpdateUsage'
  { -- | For more information about supported patch operations, see
    -- <https://docs.aws.amazon.com/apigateway/latest/api/patch-operations.html Patch Operations>.
    patchOperations :: Prelude.Maybe [PatchOperation],
    -- | The Id of the usage plan associated with the usage data.
    usagePlanId :: Prelude.Text,
    -- | The identifier of the API key associated with the usage plan in which a
    -- temporary extension is granted to the remaining quota.
    keyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateUsage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'patchOperations', 'updateUsage_patchOperations' - For more information about supported patch operations, see
-- <https://docs.aws.amazon.com/apigateway/latest/api/patch-operations.html Patch Operations>.
--
-- 'usagePlanId', 'updateUsage_usagePlanId' - The Id of the usage plan associated with the usage data.
--
-- 'keyId', 'updateUsage_keyId' - The identifier of the API key associated with the usage plan in which a
-- temporary extension is granted to the remaining quota.
newUpdateUsage ::
  -- | 'usagePlanId'
  Prelude.Text ->
  -- | 'keyId'
  Prelude.Text ->
  UpdateUsage
newUpdateUsage pUsagePlanId_ pKeyId_ =
  UpdateUsage'
    { patchOperations = Prelude.Nothing,
      usagePlanId = pUsagePlanId_,
      keyId = pKeyId_
    }

-- | For more information about supported patch operations, see
-- <https://docs.aws.amazon.com/apigateway/latest/api/patch-operations.html Patch Operations>.
updateUsage_patchOperations :: Lens.Lens' UpdateUsage (Prelude.Maybe [PatchOperation])
updateUsage_patchOperations = Lens.lens (\UpdateUsage' {patchOperations} -> patchOperations) (\s@UpdateUsage' {} a -> s {patchOperations = a} :: UpdateUsage) Prelude.. Lens.mapping Lens.coerced

-- | The Id of the usage plan associated with the usage data.
updateUsage_usagePlanId :: Lens.Lens' UpdateUsage Prelude.Text
updateUsage_usagePlanId = Lens.lens (\UpdateUsage' {usagePlanId} -> usagePlanId) (\s@UpdateUsage' {} a -> s {usagePlanId = a} :: UpdateUsage)

-- | The identifier of the API key associated with the usage plan in which a
-- temporary extension is granted to the remaining quota.
updateUsage_keyId :: Lens.Lens' UpdateUsage Prelude.Text
updateUsage_keyId = Lens.lens (\UpdateUsage' {keyId} -> keyId) (\s@UpdateUsage' {} a -> s {keyId = a} :: UpdateUsage)

instance Core.AWSRequest UpdateUsage where
  type AWSResponse UpdateUsage = Usage
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable UpdateUsage where
  hashWithSalt _salt UpdateUsage' {..} =
    _salt `Prelude.hashWithSalt` patchOperations
      `Prelude.hashWithSalt` usagePlanId
      `Prelude.hashWithSalt` keyId

instance Prelude.NFData UpdateUsage where
  rnf UpdateUsage' {..} =
    Prelude.rnf patchOperations
      `Prelude.seq` Prelude.rnf usagePlanId
      `Prelude.seq` Prelude.rnf keyId

instance Data.ToHeaders UpdateUsage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToJSON UpdateUsage where
  toJSON UpdateUsage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("patchOperations" Data..=)
              Prelude.<$> patchOperations
          ]
      )

instance Data.ToPath UpdateUsage where
  toPath UpdateUsage' {..} =
    Prelude.mconcat
      [ "/usageplans/",
        Data.toBS usagePlanId,
        "/keys/",
        Data.toBS keyId,
        "/usage"
      ]

instance Data.ToQuery UpdateUsage where
  toQuery = Prelude.const Prelude.mempty
