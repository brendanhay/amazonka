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
-- Module      : Amazonka.IoT.UpdateThingGroupsForThing
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the groups to which the thing belongs.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions UpdateThingGroupsForThing>
-- action.
module Amazonka.IoT.UpdateThingGroupsForThing
  ( -- * Creating a Request
    UpdateThingGroupsForThing (..),
    newUpdateThingGroupsForThing,

    -- * Request Lenses
    updateThingGroupsForThing_overrideDynamicGroups,
    updateThingGroupsForThing_thingGroupsToAdd,
    updateThingGroupsForThing_thingGroupsToRemove,
    updateThingGroupsForThing_thingName,

    -- * Destructuring the Response
    UpdateThingGroupsForThingResponse (..),
    newUpdateThingGroupsForThingResponse,

    -- * Response Lenses
    updateThingGroupsForThingResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateThingGroupsForThing' smart constructor.
data UpdateThingGroupsForThing = UpdateThingGroupsForThing'
  { -- | Override dynamic thing groups with static thing groups when 10-group
    -- limit is reached. If a thing belongs to 10 thing groups, and one or more
    -- of those groups are dynamic thing groups, adding a thing to a static
    -- group removes the thing from the last dynamic group.
    overrideDynamicGroups :: Prelude.Maybe Prelude.Bool,
    -- | The groups to which the thing will be added.
    thingGroupsToAdd :: Prelude.Maybe [Prelude.Text],
    -- | The groups from which the thing will be removed.
    thingGroupsToRemove :: Prelude.Maybe [Prelude.Text],
    -- | The thing whose group memberships will be updated.
    thingName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateThingGroupsForThing' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'overrideDynamicGroups', 'updateThingGroupsForThing_overrideDynamicGroups' - Override dynamic thing groups with static thing groups when 10-group
-- limit is reached. If a thing belongs to 10 thing groups, and one or more
-- of those groups are dynamic thing groups, adding a thing to a static
-- group removes the thing from the last dynamic group.
--
-- 'thingGroupsToAdd', 'updateThingGroupsForThing_thingGroupsToAdd' - The groups to which the thing will be added.
--
-- 'thingGroupsToRemove', 'updateThingGroupsForThing_thingGroupsToRemove' - The groups from which the thing will be removed.
--
-- 'thingName', 'updateThingGroupsForThing_thingName' - The thing whose group memberships will be updated.
newUpdateThingGroupsForThing ::
  UpdateThingGroupsForThing
newUpdateThingGroupsForThing =
  UpdateThingGroupsForThing'
    { overrideDynamicGroups =
        Prelude.Nothing,
      thingGroupsToAdd = Prelude.Nothing,
      thingGroupsToRemove = Prelude.Nothing,
      thingName = Prelude.Nothing
    }

-- | Override dynamic thing groups with static thing groups when 10-group
-- limit is reached. If a thing belongs to 10 thing groups, and one or more
-- of those groups are dynamic thing groups, adding a thing to a static
-- group removes the thing from the last dynamic group.
updateThingGroupsForThing_overrideDynamicGroups :: Lens.Lens' UpdateThingGroupsForThing (Prelude.Maybe Prelude.Bool)
updateThingGroupsForThing_overrideDynamicGroups = Lens.lens (\UpdateThingGroupsForThing' {overrideDynamicGroups} -> overrideDynamicGroups) (\s@UpdateThingGroupsForThing' {} a -> s {overrideDynamicGroups = a} :: UpdateThingGroupsForThing)

-- | The groups to which the thing will be added.
updateThingGroupsForThing_thingGroupsToAdd :: Lens.Lens' UpdateThingGroupsForThing (Prelude.Maybe [Prelude.Text])
updateThingGroupsForThing_thingGroupsToAdd = Lens.lens (\UpdateThingGroupsForThing' {thingGroupsToAdd} -> thingGroupsToAdd) (\s@UpdateThingGroupsForThing' {} a -> s {thingGroupsToAdd = a} :: UpdateThingGroupsForThing) Prelude.. Lens.mapping Lens.coerced

-- | The groups from which the thing will be removed.
updateThingGroupsForThing_thingGroupsToRemove :: Lens.Lens' UpdateThingGroupsForThing (Prelude.Maybe [Prelude.Text])
updateThingGroupsForThing_thingGroupsToRemove = Lens.lens (\UpdateThingGroupsForThing' {thingGroupsToRemove} -> thingGroupsToRemove) (\s@UpdateThingGroupsForThing' {} a -> s {thingGroupsToRemove = a} :: UpdateThingGroupsForThing) Prelude.. Lens.mapping Lens.coerced

-- | The thing whose group memberships will be updated.
updateThingGroupsForThing_thingName :: Lens.Lens' UpdateThingGroupsForThing (Prelude.Maybe Prelude.Text)
updateThingGroupsForThing_thingName = Lens.lens (\UpdateThingGroupsForThing' {thingName} -> thingName) (\s@UpdateThingGroupsForThing' {} a -> s {thingName = a} :: UpdateThingGroupsForThing)

instance Core.AWSRequest UpdateThingGroupsForThing where
  type
    AWSResponse UpdateThingGroupsForThing =
      UpdateThingGroupsForThingResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateThingGroupsForThingResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateThingGroupsForThing where
  hashWithSalt _salt UpdateThingGroupsForThing' {..} =
    _salt `Prelude.hashWithSalt` overrideDynamicGroups
      `Prelude.hashWithSalt` thingGroupsToAdd
      `Prelude.hashWithSalt` thingGroupsToRemove
      `Prelude.hashWithSalt` thingName

instance Prelude.NFData UpdateThingGroupsForThing where
  rnf UpdateThingGroupsForThing' {..} =
    Prelude.rnf overrideDynamicGroups
      `Prelude.seq` Prelude.rnf thingGroupsToAdd
      `Prelude.seq` Prelude.rnf thingGroupsToRemove
      `Prelude.seq` Prelude.rnf thingName

instance Data.ToHeaders UpdateThingGroupsForThing where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdateThingGroupsForThing where
  toJSON UpdateThingGroupsForThing' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("overrideDynamicGroups" Data..=)
              Prelude.<$> overrideDynamicGroups,
            ("thingGroupsToAdd" Data..=)
              Prelude.<$> thingGroupsToAdd,
            ("thingGroupsToRemove" Data..=)
              Prelude.<$> thingGroupsToRemove,
            ("thingName" Data..=) Prelude.<$> thingName
          ]
      )

instance Data.ToPath UpdateThingGroupsForThing where
  toPath =
    Prelude.const
      "/thing-groups/updateThingGroupsForThing"

instance Data.ToQuery UpdateThingGroupsForThing where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateThingGroupsForThingResponse' smart constructor.
data UpdateThingGroupsForThingResponse = UpdateThingGroupsForThingResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateThingGroupsForThingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateThingGroupsForThingResponse_httpStatus' - The response's http status code.
newUpdateThingGroupsForThingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateThingGroupsForThingResponse
newUpdateThingGroupsForThingResponse pHttpStatus_ =
  UpdateThingGroupsForThingResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateThingGroupsForThingResponse_httpStatus :: Lens.Lens' UpdateThingGroupsForThingResponse Prelude.Int
updateThingGroupsForThingResponse_httpStatus = Lens.lens (\UpdateThingGroupsForThingResponse' {httpStatus} -> httpStatus) (\s@UpdateThingGroupsForThingResponse' {} a -> s {httpStatus = a} :: UpdateThingGroupsForThingResponse)

instance
  Prelude.NFData
    UpdateThingGroupsForThingResponse
  where
  rnf UpdateThingGroupsForThingResponse' {..} =
    Prelude.rnf httpStatus
