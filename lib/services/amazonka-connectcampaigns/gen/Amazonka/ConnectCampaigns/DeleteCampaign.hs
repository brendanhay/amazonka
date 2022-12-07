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
-- Module      : Amazonka.ConnectCampaigns.DeleteCampaign
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a campaign from the specified Amazon Connect account.
module Amazonka.ConnectCampaigns.DeleteCampaign
  ( -- * Creating a Request
    DeleteCampaign (..),
    newDeleteCampaign,

    -- * Request Lenses
    deleteCampaign_id,

    -- * Destructuring the Response
    DeleteCampaignResponse (..),
    newDeleteCampaignResponse,
  )
where

import Amazonka.ConnectCampaigns.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | DeleteCampaignRequest
--
-- /See:/ 'newDeleteCampaign' smart constructor.
data DeleteCampaign = DeleteCampaign'
  { id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCampaign' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deleteCampaign_id' - Undocumented member.
newDeleteCampaign ::
  -- | 'id'
  Prelude.Text ->
  DeleteCampaign
newDeleteCampaign pId_ = DeleteCampaign' {id = pId_}

-- | Undocumented member.
deleteCampaign_id :: Lens.Lens' DeleteCampaign Prelude.Text
deleteCampaign_id = Lens.lens (\DeleteCampaign' {id} -> id) (\s@DeleteCampaign' {} a -> s {id = a} :: DeleteCampaign)

instance Core.AWSRequest DeleteCampaign where
  type
    AWSResponse DeleteCampaign =
      DeleteCampaignResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteCampaignResponse'

instance Prelude.Hashable DeleteCampaign where
  hashWithSalt _salt DeleteCampaign' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData DeleteCampaign where
  rnf DeleteCampaign' {..} = Prelude.rnf id

instance Data.ToHeaders DeleteCampaign where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteCampaign where
  toPath DeleteCampaign' {..} =
    Prelude.mconcat ["/campaigns/", Data.toBS id]

instance Data.ToQuery DeleteCampaign where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteCampaignResponse' smart constructor.
data DeleteCampaignResponse = DeleteCampaignResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCampaignResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteCampaignResponse ::
  DeleteCampaignResponse
newDeleteCampaignResponse = DeleteCampaignResponse'

instance Prelude.NFData DeleteCampaignResponse where
  rnf _ = ()
