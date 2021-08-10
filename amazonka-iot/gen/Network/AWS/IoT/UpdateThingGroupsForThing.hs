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
-- Module      : Network.AWS.IoT.UpdateThingGroupsForThing
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the groups to which the thing belongs.
module Network.AWS.IoT.UpdateThingGroupsForThing
  ( -- * Creating a Request
    UpdateThingGroupsForThing (..),
    newUpdateThingGroupsForThing,

    -- * Request Lenses
    updateThingGroupsForThing_thingName,
    updateThingGroupsForThing_overrideDynamicGroups,
    updateThingGroupsForThing_thingGroupsToRemove,
    updateThingGroupsForThing_thingGroupsToAdd,

    -- * Destructuring the Response
    UpdateThingGroupsForThingResponse (..),
    newUpdateThingGroupsForThingResponse,

    -- * Response Lenses
    updateThingGroupsForThingResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateThingGroupsForThing' smart constructor.
data UpdateThingGroupsForThing = UpdateThingGroupsForThing'
  { -- | The thing whose group memberships will be updated.
    thingName :: Prelude.Maybe Prelude.Text,
    -- | Override dynamic thing groups with static thing groups when 10-group
    -- limit is reached. If a thing belongs to 10 thing groups, and one or more
    -- of those groups are dynamic thing groups, adding a thing to a static
    -- group removes the thing from the last dynamic group.
    overrideDynamicGroups :: Prelude.Maybe Prelude.Bool,
    -- | The groups from which the thing will be removed.
    thingGroupsToRemove :: Prelude.Maybe [Prelude.Text],
    -- | The groups to which the thing will be added.
    thingGroupsToAdd :: Prelude.Maybe [Prelude.Text]
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
-- 'thingName', 'updateThingGroupsForThing_thingName' - The thing whose group memberships will be updated.
--
-- 'overrideDynamicGroups', 'updateThingGroupsForThing_overrideDynamicGroups' - Override dynamic thing groups with static thing groups when 10-group
-- limit is reached. If a thing belongs to 10 thing groups, and one or more
-- of those groups are dynamic thing groups, adding a thing to a static
-- group removes the thing from the last dynamic group.
--
-- 'thingGroupsToRemove', 'updateThingGroupsForThing_thingGroupsToRemove' - The groups from which the thing will be removed.
--
-- 'thingGroupsToAdd', 'updateThingGroupsForThing_thingGroupsToAdd' - The groups to which the thing will be added.
newUpdateThingGroupsForThing ::
  UpdateThingGroupsForThing
newUpdateThingGroupsForThing =
  UpdateThingGroupsForThing'
    { thingName =
        Prelude.Nothing,
      overrideDynamicGroups = Prelude.Nothing,
      thingGroupsToRemove = Prelude.Nothing,
      thingGroupsToAdd = Prelude.Nothing
    }

-- | The thing whose group memberships will be updated.
updateThingGroupsForThing_thingName :: Lens.Lens' UpdateThingGroupsForThing (Prelude.Maybe Prelude.Text)
updateThingGroupsForThing_thingName = Lens.lens (\UpdateThingGroupsForThing' {thingName} -> thingName) (\s@UpdateThingGroupsForThing' {} a -> s {thingName = a} :: UpdateThingGroupsForThing)

-- | Override dynamic thing groups with static thing groups when 10-group
-- limit is reached. If a thing belongs to 10 thing groups, and one or more
-- of those groups are dynamic thing groups, adding a thing to a static
-- group removes the thing from the last dynamic group.
updateThingGroupsForThing_overrideDynamicGroups :: Lens.Lens' UpdateThingGroupsForThing (Prelude.Maybe Prelude.Bool)
updateThingGroupsForThing_overrideDynamicGroups = Lens.lens (\UpdateThingGroupsForThing' {overrideDynamicGroups} -> overrideDynamicGroups) (\s@UpdateThingGroupsForThing' {} a -> s {overrideDynamicGroups = a} :: UpdateThingGroupsForThing)

-- | The groups from which the thing will be removed.
updateThingGroupsForThing_thingGroupsToRemove :: Lens.Lens' UpdateThingGroupsForThing (Prelude.Maybe [Prelude.Text])
updateThingGroupsForThing_thingGroupsToRemove = Lens.lens (\UpdateThingGroupsForThing' {thingGroupsToRemove} -> thingGroupsToRemove) (\s@UpdateThingGroupsForThing' {} a -> s {thingGroupsToRemove = a} :: UpdateThingGroupsForThing) Prelude.. Lens.mapping Lens._Coerce

-- | The groups to which the thing will be added.
updateThingGroupsForThing_thingGroupsToAdd :: Lens.Lens' UpdateThingGroupsForThing (Prelude.Maybe [Prelude.Text])
updateThingGroupsForThing_thingGroupsToAdd = Lens.lens (\UpdateThingGroupsForThing' {thingGroupsToAdd} -> thingGroupsToAdd) (\s@UpdateThingGroupsForThing' {} a -> s {thingGroupsToAdd = a} :: UpdateThingGroupsForThing) Prelude.. Lens.mapping Lens._Coerce

instance Core.AWSRequest UpdateThingGroupsForThing where
  type
    AWSResponse UpdateThingGroupsForThing =
      UpdateThingGroupsForThingResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateThingGroupsForThingResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateThingGroupsForThing

instance Prelude.NFData UpdateThingGroupsForThing

instance Core.ToHeaders UpdateThingGroupsForThing where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON UpdateThingGroupsForThing where
  toJSON UpdateThingGroupsForThing' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("thingName" Core..=) Prelude.<$> thingName,
            ("overrideDynamicGroups" Core..=)
              Prelude.<$> overrideDynamicGroups,
            ("thingGroupsToRemove" Core..=)
              Prelude.<$> thingGroupsToRemove,
            ("thingGroupsToAdd" Core..=)
              Prelude.<$> thingGroupsToAdd
          ]
      )

instance Core.ToPath UpdateThingGroupsForThing where
  toPath =
    Prelude.const
      "/thing-groups/updateThingGroupsForThing"

instance Core.ToQuery UpdateThingGroupsForThing where
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
