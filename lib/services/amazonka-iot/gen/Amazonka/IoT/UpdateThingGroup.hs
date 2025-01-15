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
-- Module      : Amazonka.IoT.UpdateThingGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update a thing group.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions UpdateThingGroup>
-- action.
module Amazonka.IoT.UpdateThingGroup
  ( -- * Creating a Request
    UpdateThingGroup (..),
    newUpdateThingGroup,

    -- * Request Lenses
    updateThingGroup_expectedVersion,
    updateThingGroup_thingGroupName,
    updateThingGroup_thingGroupProperties,

    -- * Destructuring the Response
    UpdateThingGroupResponse (..),
    newUpdateThingGroupResponse,

    -- * Response Lenses
    updateThingGroupResponse_version,
    updateThingGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateThingGroup' smart constructor.
data UpdateThingGroup = UpdateThingGroup'
  { -- | The expected version of the thing group. If this does not match the
    -- version of the thing group being updated, the update will fail.
    expectedVersion :: Prelude.Maybe Prelude.Integer,
    -- | The thing group to update.
    thingGroupName :: Prelude.Text,
    -- | The thing group properties.
    thingGroupProperties :: ThingGroupProperties
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateThingGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedVersion', 'updateThingGroup_expectedVersion' - The expected version of the thing group. If this does not match the
-- version of the thing group being updated, the update will fail.
--
-- 'thingGroupName', 'updateThingGroup_thingGroupName' - The thing group to update.
--
-- 'thingGroupProperties', 'updateThingGroup_thingGroupProperties' - The thing group properties.
newUpdateThingGroup ::
  -- | 'thingGroupName'
  Prelude.Text ->
  -- | 'thingGroupProperties'
  ThingGroupProperties ->
  UpdateThingGroup
newUpdateThingGroup
  pThingGroupName_
  pThingGroupProperties_ =
    UpdateThingGroup'
      { expectedVersion =
          Prelude.Nothing,
        thingGroupName = pThingGroupName_,
        thingGroupProperties = pThingGroupProperties_
      }

-- | The expected version of the thing group. If this does not match the
-- version of the thing group being updated, the update will fail.
updateThingGroup_expectedVersion :: Lens.Lens' UpdateThingGroup (Prelude.Maybe Prelude.Integer)
updateThingGroup_expectedVersion = Lens.lens (\UpdateThingGroup' {expectedVersion} -> expectedVersion) (\s@UpdateThingGroup' {} a -> s {expectedVersion = a} :: UpdateThingGroup)

-- | The thing group to update.
updateThingGroup_thingGroupName :: Lens.Lens' UpdateThingGroup Prelude.Text
updateThingGroup_thingGroupName = Lens.lens (\UpdateThingGroup' {thingGroupName} -> thingGroupName) (\s@UpdateThingGroup' {} a -> s {thingGroupName = a} :: UpdateThingGroup)

-- | The thing group properties.
updateThingGroup_thingGroupProperties :: Lens.Lens' UpdateThingGroup ThingGroupProperties
updateThingGroup_thingGroupProperties = Lens.lens (\UpdateThingGroup' {thingGroupProperties} -> thingGroupProperties) (\s@UpdateThingGroup' {} a -> s {thingGroupProperties = a} :: UpdateThingGroup)

instance Core.AWSRequest UpdateThingGroup where
  type
    AWSResponse UpdateThingGroup =
      UpdateThingGroupResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateThingGroupResponse'
            Prelude.<$> (x Data..?> "version")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateThingGroup where
  hashWithSalt _salt UpdateThingGroup' {..} =
    _salt
      `Prelude.hashWithSalt` expectedVersion
      `Prelude.hashWithSalt` thingGroupName
      `Prelude.hashWithSalt` thingGroupProperties

instance Prelude.NFData UpdateThingGroup where
  rnf UpdateThingGroup' {..} =
    Prelude.rnf expectedVersion `Prelude.seq`
      Prelude.rnf thingGroupName `Prelude.seq`
        Prelude.rnf thingGroupProperties

instance Data.ToHeaders UpdateThingGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdateThingGroup where
  toJSON UpdateThingGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("expectedVersion" Data..=)
              Prelude.<$> expectedVersion,
            Prelude.Just
              ( "thingGroupProperties"
                  Data..= thingGroupProperties
              )
          ]
      )

instance Data.ToPath UpdateThingGroup where
  toPath UpdateThingGroup' {..} =
    Prelude.mconcat
      ["/thing-groups/", Data.toBS thingGroupName]

instance Data.ToQuery UpdateThingGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateThingGroupResponse' smart constructor.
data UpdateThingGroupResponse = UpdateThingGroupResponse'
  { -- | The version of the updated thing group.
    version :: Prelude.Maybe Prelude.Integer,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateThingGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'version', 'updateThingGroupResponse_version' - The version of the updated thing group.
--
-- 'httpStatus', 'updateThingGroupResponse_httpStatus' - The response's http status code.
newUpdateThingGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateThingGroupResponse
newUpdateThingGroupResponse pHttpStatus_ =
  UpdateThingGroupResponse'
    { version =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The version of the updated thing group.
updateThingGroupResponse_version :: Lens.Lens' UpdateThingGroupResponse (Prelude.Maybe Prelude.Integer)
updateThingGroupResponse_version = Lens.lens (\UpdateThingGroupResponse' {version} -> version) (\s@UpdateThingGroupResponse' {} a -> s {version = a} :: UpdateThingGroupResponse)

-- | The response's http status code.
updateThingGroupResponse_httpStatus :: Lens.Lens' UpdateThingGroupResponse Prelude.Int
updateThingGroupResponse_httpStatus = Lens.lens (\UpdateThingGroupResponse' {httpStatus} -> httpStatus) (\s@UpdateThingGroupResponse' {} a -> s {httpStatus = a} :: UpdateThingGroupResponse)

instance Prelude.NFData UpdateThingGroupResponse where
  rnf UpdateThingGroupResponse' {..} =
    Prelude.rnf version `Prelude.seq`
      Prelude.rnf httpStatus
