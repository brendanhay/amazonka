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
-- Module      : Network.AWS.APIGateway.UpdateUsage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Grants a temporary extension to the remaining quota of a usage plan
-- associated with a specified API key.
module Network.AWS.APIGateway.UpdateUsage
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
    usage_startDate,
    usage_items,
    usage_position,
    usage_endDate,
    usage_usagePlanId,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The PATCH request to grant a temporary extension to the remaining quota
-- of a usage plan associated with a specified API key.
--
-- /See:/ 'newUpdateUsage' smart constructor.
data UpdateUsage = UpdateUsage'
  { -- | A list of update operations to be applied to the specified resource and
    -- in the order specified in this list.
    patchOperations :: Prelude.Maybe [PatchOperation],
    -- | [Required] The Id of the usage plan associated with the usage data.
    usagePlanId :: Prelude.Text,
    -- | [Required] The identifier of the API key associated with the usage plan
    -- in which a temporary extension is granted to the remaining quota.
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
-- 'patchOperations', 'updateUsage_patchOperations' - A list of update operations to be applied to the specified resource and
-- in the order specified in this list.
--
-- 'usagePlanId', 'updateUsage_usagePlanId' - [Required] The Id of the usage plan associated with the usage data.
--
-- 'keyId', 'updateUsage_keyId' - [Required] The identifier of the API key associated with the usage plan
-- in which a temporary extension is granted to the remaining quota.
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

-- | A list of update operations to be applied to the specified resource and
-- in the order specified in this list.
updateUsage_patchOperations :: Lens.Lens' UpdateUsage (Prelude.Maybe [PatchOperation])
updateUsage_patchOperations = Lens.lens (\UpdateUsage' {patchOperations} -> patchOperations) (\s@UpdateUsage' {} a -> s {patchOperations = a} :: UpdateUsage) Prelude.. Lens.mapping Lens._Coerce

-- | [Required] The Id of the usage plan associated with the usage data.
updateUsage_usagePlanId :: Lens.Lens' UpdateUsage Prelude.Text
updateUsage_usagePlanId = Lens.lens (\UpdateUsage' {usagePlanId} -> usagePlanId) (\s@UpdateUsage' {} a -> s {usagePlanId = a} :: UpdateUsage)

-- | [Required] The identifier of the API key associated with the usage plan
-- in which a temporary extension is granted to the remaining quota.
updateUsage_keyId :: Lens.Lens' UpdateUsage Prelude.Text
updateUsage_keyId = Lens.lens (\UpdateUsage' {keyId} -> keyId) (\s@UpdateUsage' {} a -> s {keyId = a} :: UpdateUsage)

instance Core.AWSRequest UpdateUsage where
  type AWSResponse UpdateUsage = Usage
  request = Request.patchJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable UpdateUsage

instance Prelude.NFData UpdateUsage

instance Core.ToHeaders UpdateUsage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Core.ToJSON UpdateUsage where
  toJSON UpdateUsage' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("patchOperations" Core..=)
              Prelude.<$> patchOperations
          ]
      )

instance Core.ToPath UpdateUsage where
  toPath UpdateUsage' {..} =
    Prelude.mconcat
      [ "/usageplans/",
        Core.toBS usagePlanId,
        "/keys/",
        Core.toBS keyId,
        "/usage"
      ]

instance Core.ToQuery UpdateUsage where
  toQuery = Prelude.const Prelude.mempty
