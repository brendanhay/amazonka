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
-- Module      : Network.AWS.IoT.UpdateThingGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update a thing group.
module Network.AWS.IoT.UpdateThingGroup
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

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  request = Request.patchJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateThingGroupResponse'
            Prelude.<$> (x Core..?> "version")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateThingGroup

instance Prelude.NFData UpdateThingGroup

instance Core.ToHeaders UpdateThingGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON UpdateThingGroup where
  toJSON UpdateThingGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("expectedVersion" Core..=)
              Prelude.<$> expectedVersion,
            Prelude.Just
              ( "thingGroupProperties"
                  Core..= thingGroupProperties
              )
          ]
      )

instance Core.ToPath UpdateThingGroup where
  toPath UpdateThingGroup' {..} =
    Prelude.mconcat
      ["/thing-groups/", Core.toBS thingGroupName]

instance Core.ToQuery UpdateThingGroup where
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

instance Prelude.NFData UpdateThingGroupResponse
