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
-- Module      : Amazonka.ConnectCampaigns.UpdateCampaignName
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the name of a campaign. This API is idempotent.
module Amazonka.ConnectCampaigns.UpdateCampaignName
  ( -- * Creating a Request
    UpdateCampaignName (..),
    newUpdateCampaignName,

    -- * Request Lenses
    updateCampaignName_id,
    updateCampaignName_name,

    -- * Destructuring the Response
    UpdateCampaignNameResponse (..),
    newUpdateCampaignNameResponse,
  )
where

import Amazonka.ConnectCampaigns.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | UpdateCampaignNameRequest
--
-- /See:/ 'newUpdateCampaignName' smart constructor.
data UpdateCampaignName = UpdateCampaignName'
  { id :: Prelude.Text,
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCampaignName' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'updateCampaignName_id' - Undocumented member.
--
-- 'name', 'updateCampaignName_name' - Undocumented member.
newUpdateCampaignName ::
  -- | 'id'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  UpdateCampaignName
newUpdateCampaignName pId_ pName_ =
  UpdateCampaignName' {id = pId_, name = pName_}

-- | Undocumented member.
updateCampaignName_id :: Lens.Lens' UpdateCampaignName Prelude.Text
updateCampaignName_id = Lens.lens (\UpdateCampaignName' {id} -> id) (\s@UpdateCampaignName' {} a -> s {id = a} :: UpdateCampaignName)

-- | Undocumented member.
updateCampaignName_name :: Lens.Lens' UpdateCampaignName Prelude.Text
updateCampaignName_name = Lens.lens (\UpdateCampaignName' {name} -> name) (\s@UpdateCampaignName' {} a -> s {name = a} :: UpdateCampaignName)

instance Core.AWSRequest UpdateCampaignName where
  type
    AWSResponse UpdateCampaignName =
      UpdateCampaignNameResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull UpdateCampaignNameResponse'

instance Prelude.Hashable UpdateCampaignName where
  hashWithSalt _salt UpdateCampaignName' {..} =
    _salt `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name

instance Prelude.NFData UpdateCampaignName where
  rnf UpdateCampaignName' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf name

instance Core.ToHeaders UpdateCampaignName where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateCampaignName where
  toJSON UpdateCampaignName' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("name" Core..= name)]
      )

instance Core.ToPath UpdateCampaignName where
  toPath UpdateCampaignName' {..} =
    Prelude.mconcat
      ["/campaigns/", Core.toBS id, "/name"]

instance Core.ToQuery UpdateCampaignName where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateCampaignNameResponse' smart constructor.
data UpdateCampaignNameResponse = UpdateCampaignNameResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCampaignNameResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateCampaignNameResponse ::
  UpdateCampaignNameResponse
newUpdateCampaignNameResponse =
  UpdateCampaignNameResponse'

instance Prelude.NFData UpdateCampaignNameResponse where
  rnf _ = ()
