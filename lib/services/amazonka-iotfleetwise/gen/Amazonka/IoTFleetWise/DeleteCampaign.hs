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
-- Module      : Amazonka.IoTFleetWise.DeleteCampaign
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a data collection campaign. Deleting a campaign suspends all
-- data collection and removes it from any vehicles.
module Amazonka.IoTFleetWise.DeleteCampaign
  ( -- * Creating a Request
    DeleteCampaign (..),
    newDeleteCampaign,

    -- * Request Lenses
    deleteCampaign_name,

    -- * Destructuring the Response
    DeleteCampaignResponse (..),
    newDeleteCampaignResponse,

    -- * Response Lenses
    deleteCampaignResponse_name,
    deleteCampaignResponse_arn,
    deleteCampaignResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTFleetWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteCampaign' smart constructor.
data DeleteCampaign = DeleteCampaign'
  { -- | The name of the campaign to delete.
    name :: Prelude.Text
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
-- 'name', 'deleteCampaign_name' - The name of the campaign to delete.
newDeleteCampaign ::
  -- | 'name'
  Prelude.Text ->
  DeleteCampaign
newDeleteCampaign pName_ =
  DeleteCampaign' {name = pName_}

-- | The name of the campaign to delete.
deleteCampaign_name :: Lens.Lens' DeleteCampaign Prelude.Text
deleteCampaign_name = Lens.lens (\DeleteCampaign' {name} -> name) (\s@DeleteCampaign' {} a -> s {name = a} :: DeleteCampaign)

instance Core.AWSRequest DeleteCampaign where
  type
    AWSResponse DeleteCampaign =
      DeleteCampaignResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteCampaignResponse'
            Prelude.<$> (x Core..?> "name")
            Prelude.<*> (x Core..?> "arn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteCampaign where
  hashWithSalt _salt DeleteCampaign' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteCampaign where
  rnf DeleteCampaign' {..} = Prelude.rnf name

instance Core.ToHeaders DeleteCampaign where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "IoTAutobahnControlPlane.DeleteCampaign" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteCampaign where
  toJSON DeleteCampaign' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("name" Core..= name)]
      )

instance Core.ToPath DeleteCampaign where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteCampaign where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteCampaignResponse' smart constructor.
data DeleteCampaignResponse = DeleteCampaignResponse'
  { -- | The name of the deleted campaign.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the deleted campaign.
    --
    -- The ARN isn’t returned if a campaign doesn’t exist.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCampaignResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteCampaignResponse_name' - The name of the deleted campaign.
--
-- 'arn', 'deleteCampaignResponse_arn' - The Amazon Resource Name (ARN) of the deleted campaign.
--
-- The ARN isn’t returned if a campaign doesn’t exist.
--
-- 'httpStatus', 'deleteCampaignResponse_httpStatus' - The response's http status code.
newDeleteCampaignResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteCampaignResponse
newDeleteCampaignResponse pHttpStatus_ =
  DeleteCampaignResponse'
    { name = Prelude.Nothing,
      arn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the deleted campaign.
deleteCampaignResponse_name :: Lens.Lens' DeleteCampaignResponse (Prelude.Maybe Prelude.Text)
deleteCampaignResponse_name = Lens.lens (\DeleteCampaignResponse' {name} -> name) (\s@DeleteCampaignResponse' {} a -> s {name = a} :: DeleteCampaignResponse)

-- | The Amazon Resource Name (ARN) of the deleted campaign.
--
-- The ARN isn’t returned if a campaign doesn’t exist.
deleteCampaignResponse_arn :: Lens.Lens' DeleteCampaignResponse (Prelude.Maybe Prelude.Text)
deleteCampaignResponse_arn = Lens.lens (\DeleteCampaignResponse' {arn} -> arn) (\s@DeleteCampaignResponse' {} a -> s {arn = a} :: DeleteCampaignResponse)

-- | The response's http status code.
deleteCampaignResponse_httpStatus :: Lens.Lens' DeleteCampaignResponse Prelude.Int
deleteCampaignResponse_httpStatus = Lens.lens (\DeleteCampaignResponse' {httpStatus} -> httpStatus) (\s@DeleteCampaignResponse' {} a -> s {httpStatus = a} :: DeleteCampaignResponse)

instance Prelude.NFData DeleteCampaignResponse where
  rnf DeleteCampaignResponse' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf httpStatus
