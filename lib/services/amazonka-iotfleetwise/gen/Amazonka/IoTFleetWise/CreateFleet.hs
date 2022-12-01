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
-- Module      : Amazonka.IoTFleetWise.CreateFleet
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a fleet that represents a group of vehicles.
--
-- You must create both a signal catalog and vehicles before you can create
-- a fleet.
--
-- For more information, see
-- <https://docs.aws.amazon.com/iot-fleetwise/latest/developerguide/fleets.html Fleets>
-- in the /Amazon Web Services IoT FleetWise Developer Guide/.
module Amazonka.IoTFleetWise.CreateFleet
  ( -- * Creating a Request
    CreateFleet (..),
    newCreateFleet,

    -- * Request Lenses
    createFleet_tags,
    createFleet_description,
    createFleet_fleetId,
    createFleet_signalCatalogArn,

    -- * Destructuring the Response
    CreateFleetResponse (..),
    newCreateFleetResponse,

    -- * Response Lenses
    createFleetResponse_httpStatus,
    createFleetResponse_id,
    createFleetResponse_arn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTFleetWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateFleet' smart constructor.
data CreateFleet = CreateFleet'
  { -- | Metadata that can be used to manage the fleet.
    tags :: Prelude.Maybe [Tag],
    -- | A brief description of the fleet to create.
    description :: Prelude.Maybe Prelude.Text,
    -- | The unique ID of the fleet to create.
    fleetId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of a signal catalog.
    signalCatalogArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFleet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createFleet_tags' - Metadata that can be used to manage the fleet.
--
-- 'description', 'createFleet_description' - A brief description of the fleet to create.
--
-- 'fleetId', 'createFleet_fleetId' - The unique ID of the fleet to create.
--
-- 'signalCatalogArn', 'createFleet_signalCatalogArn' - The Amazon Resource Name (ARN) of a signal catalog.
newCreateFleet ::
  -- | 'fleetId'
  Prelude.Text ->
  -- | 'signalCatalogArn'
  Prelude.Text ->
  CreateFleet
newCreateFleet pFleetId_ pSignalCatalogArn_ =
  CreateFleet'
    { tags = Prelude.Nothing,
      description = Prelude.Nothing,
      fleetId = pFleetId_,
      signalCatalogArn = pSignalCatalogArn_
    }

-- | Metadata that can be used to manage the fleet.
createFleet_tags :: Lens.Lens' CreateFleet (Prelude.Maybe [Tag])
createFleet_tags = Lens.lens (\CreateFleet' {tags} -> tags) (\s@CreateFleet' {} a -> s {tags = a} :: CreateFleet) Prelude.. Lens.mapping Lens.coerced

-- | A brief description of the fleet to create.
createFleet_description :: Lens.Lens' CreateFleet (Prelude.Maybe Prelude.Text)
createFleet_description = Lens.lens (\CreateFleet' {description} -> description) (\s@CreateFleet' {} a -> s {description = a} :: CreateFleet)

-- | The unique ID of the fleet to create.
createFleet_fleetId :: Lens.Lens' CreateFleet Prelude.Text
createFleet_fleetId = Lens.lens (\CreateFleet' {fleetId} -> fleetId) (\s@CreateFleet' {} a -> s {fleetId = a} :: CreateFleet)

-- | The Amazon Resource Name (ARN) of a signal catalog.
createFleet_signalCatalogArn :: Lens.Lens' CreateFleet Prelude.Text
createFleet_signalCatalogArn = Lens.lens (\CreateFleet' {signalCatalogArn} -> signalCatalogArn) (\s@CreateFleet' {} a -> s {signalCatalogArn = a} :: CreateFleet)

instance Core.AWSRequest CreateFleet where
  type AWSResponse CreateFleet = CreateFleetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFleetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "id")
            Prelude.<*> (x Core..:> "arn")
      )

instance Prelude.Hashable CreateFleet where
  hashWithSalt _salt CreateFleet' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` fleetId
      `Prelude.hashWithSalt` signalCatalogArn

instance Prelude.NFData CreateFleet where
  rnf CreateFleet' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf fleetId
      `Prelude.seq` Prelude.rnf signalCatalogArn

instance Core.ToHeaders CreateFleet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "IoTAutobahnControlPlane.CreateFleet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateFleet where
  toJSON CreateFleet' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tags" Core..=) Prelude.<$> tags,
            ("description" Core..=) Prelude.<$> description,
            Prelude.Just ("fleetId" Core..= fleetId),
            Prelude.Just
              ("signalCatalogArn" Core..= signalCatalogArn)
          ]
      )

instance Core.ToPath CreateFleet where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateFleet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateFleetResponse' smart constructor.
data CreateFleetResponse = CreateFleetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the created fleet.
    id :: Prelude.Text,
    -- | The ARN of the created fleet.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFleetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createFleetResponse_httpStatus' - The response's http status code.
--
-- 'id', 'createFleetResponse_id' - The ID of the created fleet.
--
-- 'arn', 'createFleetResponse_arn' - The ARN of the created fleet.
newCreateFleetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'id'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  CreateFleetResponse
newCreateFleetResponse pHttpStatus_ pId_ pArn_ =
  CreateFleetResponse'
    { httpStatus = pHttpStatus_,
      id = pId_,
      arn = pArn_
    }

-- | The response's http status code.
createFleetResponse_httpStatus :: Lens.Lens' CreateFleetResponse Prelude.Int
createFleetResponse_httpStatus = Lens.lens (\CreateFleetResponse' {httpStatus} -> httpStatus) (\s@CreateFleetResponse' {} a -> s {httpStatus = a} :: CreateFleetResponse)

-- | The ID of the created fleet.
createFleetResponse_id :: Lens.Lens' CreateFleetResponse Prelude.Text
createFleetResponse_id = Lens.lens (\CreateFleetResponse' {id} -> id) (\s@CreateFleetResponse' {} a -> s {id = a} :: CreateFleetResponse)

-- | The ARN of the created fleet.
createFleetResponse_arn :: Lens.Lens' CreateFleetResponse Prelude.Text
createFleetResponse_arn = Lens.lens (\CreateFleetResponse' {arn} -> arn) (\s@CreateFleetResponse' {} a -> s {arn = a} :: CreateFleetResponse)

instance Prelude.NFData CreateFleetResponse where
  rnf CreateFleetResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf arn
