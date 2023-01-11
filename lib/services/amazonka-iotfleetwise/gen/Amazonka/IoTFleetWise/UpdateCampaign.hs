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
-- Module      : Amazonka.IoTFleetWise.UpdateCampaign
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a campaign.
module Amazonka.IoTFleetWise.UpdateCampaign
  ( -- * Creating a Request
    UpdateCampaign (..),
    newUpdateCampaign,

    -- * Request Lenses
    updateCampaign_dataExtraDimensions,
    updateCampaign_description,
    updateCampaign_name,
    updateCampaign_action,

    -- * Destructuring the Response
    UpdateCampaignResponse (..),
    newUpdateCampaignResponse,

    -- * Response Lenses
    updateCampaignResponse_arn,
    updateCampaignResponse_name,
    updateCampaignResponse_status,
    updateCampaignResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTFleetWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateCampaign' smart constructor.
data UpdateCampaign = UpdateCampaign'
  { -- | A list of vehicle attributes to associate with a signal.
    --
    -- Default: An empty array
    dataExtraDimensions :: Prelude.Maybe [Prelude.Text],
    -- | The description of the campaign.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the campaign to update.
    name :: Prelude.Text,
    -- | Specifies how to update a campaign. The action can be one of the
    -- following:
    --
    -- -   @APPROVE@ - To approve delivering a data collection scheme to
    --     vehicles.
    --
    -- -   @SUSPEND@ - To suspend collecting signal data.
    --
    -- -   @RESUME@ - To resume collecting signal data.
    --
    -- -   @UPDATE@ - To update a campaign.
    action :: UpdateCampaignAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCampaign' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataExtraDimensions', 'updateCampaign_dataExtraDimensions' - A list of vehicle attributes to associate with a signal.
--
-- Default: An empty array
--
-- 'description', 'updateCampaign_description' - The description of the campaign.
--
-- 'name', 'updateCampaign_name' - The name of the campaign to update.
--
-- 'action', 'updateCampaign_action' - Specifies how to update a campaign. The action can be one of the
-- following:
--
-- -   @APPROVE@ - To approve delivering a data collection scheme to
--     vehicles.
--
-- -   @SUSPEND@ - To suspend collecting signal data.
--
-- -   @RESUME@ - To resume collecting signal data.
--
-- -   @UPDATE@ - To update a campaign.
newUpdateCampaign ::
  -- | 'name'
  Prelude.Text ->
  -- | 'action'
  UpdateCampaignAction ->
  UpdateCampaign
newUpdateCampaign pName_ pAction_ =
  UpdateCampaign'
    { dataExtraDimensions =
        Prelude.Nothing,
      description = Prelude.Nothing,
      name = pName_,
      action = pAction_
    }

-- | A list of vehicle attributes to associate with a signal.
--
-- Default: An empty array
updateCampaign_dataExtraDimensions :: Lens.Lens' UpdateCampaign (Prelude.Maybe [Prelude.Text])
updateCampaign_dataExtraDimensions = Lens.lens (\UpdateCampaign' {dataExtraDimensions} -> dataExtraDimensions) (\s@UpdateCampaign' {} a -> s {dataExtraDimensions = a} :: UpdateCampaign) Prelude.. Lens.mapping Lens.coerced

-- | The description of the campaign.
updateCampaign_description :: Lens.Lens' UpdateCampaign (Prelude.Maybe Prelude.Text)
updateCampaign_description = Lens.lens (\UpdateCampaign' {description} -> description) (\s@UpdateCampaign' {} a -> s {description = a} :: UpdateCampaign)

-- | The name of the campaign to update.
updateCampaign_name :: Lens.Lens' UpdateCampaign Prelude.Text
updateCampaign_name = Lens.lens (\UpdateCampaign' {name} -> name) (\s@UpdateCampaign' {} a -> s {name = a} :: UpdateCampaign)

-- | Specifies how to update a campaign. The action can be one of the
-- following:
--
-- -   @APPROVE@ - To approve delivering a data collection scheme to
--     vehicles.
--
-- -   @SUSPEND@ - To suspend collecting signal data.
--
-- -   @RESUME@ - To resume collecting signal data.
--
-- -   @UPDATE@ - To update a campaign.
updateCampaign_action :: Lens.Lens' UpdateCampaign UpdateCampaignAction
updateCampaign_action = Lens.lens (\UpdateCampaign' {action} -> action) (\s@UpdateCampaign' {} a -> s {action = a} :: UpdateCampaign)

instance Core.AWSRequest UpdateCampaign where
  type
    AWSResponse UpdateCampaign =
      UpdateCampaignResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateCampaignResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateCampaign where
  hashWithSalt _salt UpdateCampaign' {..} =
    _salt `Prelude.hashWithSalt` dataExtraDimensions
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` action

instance Prelude.NFData UpdateCampaign where
  rnf UpdateCampaign' {..} =
    Prelude.rnf dataExtraDimensions
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf action

instance Data.ToHeaders UpdateCampaign where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "IoTAutobahnControlPlane.UpdateCampaign" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateCampaign where
  toJSON UpdateCampaign' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("dataExtraDimensions" Data..=)
              Prelude.<$> dataExtraDimensions,
            ("description" Data..=) Prelude.<$> description,
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("action" Data..= action)
          ]
      )

instance Data.ToPath UpdateCampaign where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateCampaign where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateCampaignResponse' smart constructor.
data UpdateCampaignResponse = UpdateCampaignResponse'
  { -- | The Amazon Resource Name (ARN) of the campaign.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name of the updated campaign.
    name :: Prelude.Maybe Prelude.Text,
    -- | The state of a campaign. The status can be one of:
    --
    -- -   @CREATING@ - Amazon Web Services IoT FleetWise is processing your
    --     request to create the campaign.
    --
    -- -   @WAITING_FOR_APPROVAL@ - After a campaign is created, it enters the
    --     @WAITING_FOR_APPROVAL@ state. To allow Amazon Web Services IoT
    --     FleetWise to deploy the campaign to the target vehicle or fleet, use
    --     the API operation to approve the campaign.
    --
    -- -   @RUNNING@ - The campaign is active.
    --
    -- -   @SUSPENDED@ - The campaign is suspended. To resume the campaign, use
    --     the API operation.
    status :: Prelude.Maybe CampaignStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCampaignResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'updateCampaignResponse_arn' - The Amazon Resource Name (ARN) of the campaign.
--
-- 'name', 'updateCampaignResponse_name' - The name of the updated campaign.
--
-- 'status', 'updateCampaignResponse_status' - The state of a campaign. The status can be one of:
--
-- -   @CREATING@ - Amazon Web Services IoT FleetWise is processing your
--     request to create the campaign.
--
-- -   @WAITING_FOR_APPROVAL@ - After a campaign is created, it enters the
--     @WAITING_FOR_APPROVAL@ state. To allow Amazon Web Services IoT
--     FleetWise to deploy the campaign to the target vehicle or fleet, use
--     the API operation to approve the campaign.
--
-- -   @RUNNING@ - The campaign is active.
--
-- -   @SUSPENDED@ - The campaign is suspended. To resume the campaign, use
--     the API operation.
--
-- 'httpStatus', 'updateCampaignResponse_httpStatus' - The response's http status code.
newUpdateCampaignResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateCampaignResponse
newUpdateCampaignResponse pHttpStatus_ =
  UpdateCampaignResponse'
    { arn = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the campaign.
updateCampaignResponse_arn :: Lens.Lens' UpdateCampaignResponse (Prelude.Maybe Prelude.Text)
updateCampaignResponse_arn = Lens.lens (\UpdateCampaignResponse' {arn} -> arn) (\s@UpdateCampaignResponse' {} a -> s {arn = a} :: UpdateCampaignResponse)

-- | The name of the updated campaign.
updateCampaignResponse_name :: Lens.Lens' UpdateCampaignResponse (Prelude.Maybe Prelude.Text)
updateCampaignResponse_name = Lens.lens (\UpdateCampaignResponse' {name} -> name) (\s@UpdateCampaignResponse' {} a -> s {name = a} :: UpdateCampaignResponse)

-- | The state of a campaign. The status can be one of:
--
-- -   @CREATING@ - Amazon Web Services IoT FleetWise is processing your
--     request to create the campaign.
--
-- -   @WAITING_FOR_APPROVAL@ - After a campaign is created, it enters the
--     @WAITING_FOR_APPROVAL@ state. To allow Amazon Web Services IoT
--     FleetWise to deploy the campaign to the target vehicle or fleet, use
--     the API operation to approve the campaign.
--
-- -   @RUNNING@ - The campaign is active.
--
-- -   @SUSPENDED@ - The campaign is suspended. To resume the campaign, use
--     the API operation.
updateCampaignResponse_status :: Lens.Lens' UpdateCampaignResponse (Prelude.Maybe CampaignStatus)
updateCampaignResponse_status = Lens.lens (\UpdateCampaignResponse' {status} -> status) (\s@UpdateCampaignResponse' {} a -> s {status = a} :: UpdateCampaignResponse)

-- | The response's http status code.
updateCampaignResponse_httpStatus :: Lens.Lens' UpdateCampaignResponse Prelude.Int
updateCampaignResponse_httpStatus = Lens.lens (\UpdateCampaignResponse' {httpStatus} -> httpStatus) (\s@UpdateCampaignResponse' {} a -> s {httpStatus = a} :: UpdateCampaignResponse)

instance Prelude.NFData UpdateCampaignResponse where
  rnf UpdateCampaignResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
