{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IoT.UpdateThing
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the data for a thing.
module Network.AWS.IoT.UpdateThing
  ( -- * Creating a Request
    UpdateThing (..),
    newUpdateThing,

    -- * Request Lenses
    updateThing_expectedVersion,
    updateThing_thingTypeName,
    updateThing_removeThingType,
    updateThing_attributePayload,
    updateThing_thingName,

    -- * Destructuring the Response
    UpdateThingResponse (..),
    newUpdateThingResponse,

    -- * Response Lenses
    updateThingResponse_httpStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the UpdateThing operation.
--
-- /See:/ 'newUpdateThing' smart constructor.
data UpdateThing = UpdateThing'
  { -- | The expected version of the thing record in the registry. If the version
    -- of the record in the registry does not match the expected version
    -- specified in the request, the @UpdateThing@ request is rejected with a
    -- @VersionConflictException@.
    expectedVersion :: Prelude.Maybe Prelude.Integer,
    -- | The name of the thing type.
    thingTypeName :: Prelude.Maybe Prelude.Text,
    -- | Remove a thing type association. If __true__, the association is
    -- removed.
    removeThingType :: Prelude.Maybe Prelude.Bool,
    -- | A list of thing attributes, a JSON string containing name-value pairs.
    -- For example:
    --
    -- @{\\\"attributes\\\":{\\\"name1\\\":\\\"value2\\\"}}@
    --
    -- This data is used to add new attributes or update existing attributes.
    attributePayload :: Prelude.Maybe AttributePayload,
    -- | The name of the thing to update.
    --
    -- You can\'t change a thing\'s name. To change a thing\'s name, you must
    -- create a new thing, give it the new name, and then delete the old thing.
    thingName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateThing' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedVersion', 'updateThing_expectedVersion' - The expected version of the thing record in the registry. If the version
-- of the record in the registry does not match the expected version
-- specified in the request, the @UpdateThing@ request is rejected with a
-- @VersionConflictException@.
--
-- 'thingTypeName', 'updateThing_thingTypeName' - The name of the thing type.
--
-- 'removeThingType', 'updateThing_removeThingType' - Remove a thing type association. If __true__, the association is
-- removed.
--
-- 'attributePayload', 'updateThing_attributePayload' - A list of thing attributes, a JSON string containing name-value pairs.
-- For example:
--
-- @{\\\"attributes\\\":{\\\"name1\\\":\\\"value2\\\"}}@
--
-- This data is used to add new attributes or update existing attributes.
--
-- 'thingName', 'updateThing_thingName' - The name of the thing to update.
--
-- You can\'t change a thing\'s name. To change a thing\'s name, you must
-- create a new thing, give it the new name, and then delete the old thing.
newUpdateThing ::
  -- | 'thingName'
  Prelude.Text ->
  UpdateThing
newUpdateThing pThingName_ =
  UpdateThing'
    { expectedVersion = Prelude.Nothing,
      thingTypeName = Prelude.Nothing,
      removeThingType = Prelude.Nothing,
      attributePayload = Prelude.Nothing,
      thingName = pThingName_
    }

-- | The expected version of the thing record in the registry. If the version
-- of the record in the registry does not match the expected version
-- specified in the request, the @UpdateThing@ request is rejected with a
-- @VersionConflictException@.
updateThing_expectedVersion :: Lens.Lens' UpdateThing (Prelude.Maybe Prelude.Integer)
updateThing_expectedVersion = Lens.lens (\UpdateThing' {expectedVersion} -> expectedVersion) (\s@UpdateThing' {} a -> s {expectedVersion = a} :: UpdateThing)

-- | The name of the thing type.
updateThing_thingTypeName :: Lens.Lens' UpdateThing (Prelude.Maybe Prelude.Text)
updateThing_thingTypeName = Lens.lens (\UpdateThing' {thingTypeName} -> thingTypeName) (\s@UpdateThing' {} a -> s {thingTypeName = a} :: UpdateThing)

-- | Remove a thing type association. If __true__, the association is
-- removed.
updateThing_removeThingType :: Lens.Lens' UpdateThing (Prelude.Maybe Prelude.Bool)
updateThing_removeThingType = Lens.lens (\UpdateThing' {removeThingType} -> removeThingType) (\s@UpdateThing' {} a -> s {removeThingType = a} :: UpdateThing)

-- | A list of thing attributes, a JSON string containing name-value pairs.
-- For example:
--
-- @{\\\"attributes\\\":{\\\"name1\\\":\\\"value2\\\"}}@
--
-- This data is used to add new attributes or update existing attributes.
updateThing_attributePayload :: Lens.Lens' UpdateThing (Prelude.Maybe AttributePayload)
updateThing_attributePayload = Lens.lens (\UpdateThing' {attributePayload} -> attributePayload) (\s@UpdateThing' {} a -> s {attributePayload = a} :: UpdateThing)

-- | The name of the thing to update.
--
-- You can\'t change a thing\'s name. To change a thing\'s name, you must
-- create a new thing, give it the new name, and then delete the old thing.
updateThing_thingName :: Lens.Lens' UpdateThing Prelude.Text
updateThing_thingName = Lens.lens (\UpdateThing' {thingName} -> thingName) (\s@UpdateThing' {} a -> s {thingName = a} :: UpdateThing)

instance Prelude.AWSRequest UpdateThing where
  type Rs UpdateThing = UpdateThingResponse
  request = Request.patchJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateThingResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateThing

instance Prelude.NFData UpdateThing

instance Prelude.ToHeaders UpdateThing where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON UpdateThing where
  toJSON UpdateThing' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("expectedVersion" Prelude..=)
              Prelude.<$> expectedVersion,
            ("thingTypeName" Prelude..=)
              Prelude.<$> thingTypeName,
            ("removeThingType" Prelude..=)
              Prelude.<$> removeThingType,
            ("attributePayload" Prelude..=)
              Prelude.<$> attributePayload
          ]
      )

instance Prelude.ToPath UpdateThing where
  toPath UpdateThing' {..} =
    Prelude.mconcat
      ["/things/", Prelude.toBS thingName]

instance Prelude.ToQuery UpdateThing where
  toQuery = Prelude.const Prelude.mempty

-- | The output from the UpdateThing operation.
--
-- /See:/ 'newUpdateThingResponse' smart constructor.
data UpdateThingResponse = UpdateThingResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateThingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateThingResponse_httpStatus' - The response's http status code.
newUpdateThingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateThingResponse
newUpdateThingResponse pHttpStatus_ =
  UpdateThingResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateThingResponse_httpStatus :: Lens.Lens' UpdateThingResponse Prelude.Int
updateThingResponse_httpStatus = Lens.lens (\UpdateThingResponse' {httpStatus} -> httpStatus) (\s@UpdateThingResponse' {} a -> s {httpStatus = a} :: UpdateThingResponse)

instance Prelude.NFData UpdateThingResponse
