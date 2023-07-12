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
-- Module      : Amazonka.IoT.UpdateThing
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the data for a thing.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions UpdateThing>
-- action.
module Amazonka.IoT.UpdateThing
  ( -- * Creating a Request
    UpdateThing (..),
    newUpdateThing,

    -- * Request Lenses
    updateThing_attributePayload,
    updateThing_expectedVersion,
    updateThing_removeThingType,
    updateThing_thingTypeName,
    updateThing_thingName,

    -- * Destructuring the Response
    UpdateThingResponse (..),
    newUpdateThingResponse,

    -- * Response Lenses
    updateThingResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input for the UpdateThing operation.
--
-- /See:/ 'newUpdateThing' smart constructor.
data UpdateThing = UpdateThing'
  { -- | A list of thing attributes, a JSON string containing name-value pairs.
    -- For example:
    --
    -- @{\\\"attributes\\\":{\\\"name1\\\":\\\"value2\\\"}}@
    --
    -- This data is used to add new attributes or update existing attributes.
    attributePayload :: Prelude.Maybe AttributePayload,
    -- | The expected version of the thing record in the registry. If the version
    -- of the record in the registry does not match the expected version
    -- specified in the request, the @UpdateThing@ request is rejected with a
    -- @VersionConflictException@.
    expectedVersion :: Prelude.Maybe Prelude.Integer,
    -- | Remove a thing type association. If __true__, the association is
    -- removed.
    removeThingType :: Prelude.Maybe Prelude.Bool,
    -- | The name of the thing type.
    thingTypeName :: Prelude.Maybe Prelude.Text,
    -- | The name of the thing to update.
    --
    -- You can\'t change a thing\'s name. To change a thing\'s name, you must
    -- create a new thing, give it the new name, and then delete the old thing.
    thingName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateThing' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributePayload', 'updateThing_attributePayload' - A list of thing attributes, a JSON string containing name-value pairs.
-- For example:
--
-- @{\\\"attributes\\\":{\\\"name1\\\":\\\"value2\\\"}}@
--
-- This data is used to add new attributes or update existing attributes.
--
-- 'expectedVersion', 'updateThing_expectedVersion' - The expected version of the thing record in the registry. If the version
-- of the record in the registry does not match the expected version
-- specified in the request, the @UpdateThing@ request is rejected with a
-- @VersionConflictException@.
--
-- 'removeThingType', 'updateThing_removeThingType' - Remove a thing type association. If __true__, the association is
-- removed.
--
-- 'thingTypeName', 'updateThing_thingTypeName' - The name of the thing type.
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
    { attributePayload = Prelude.Nothing,
      expectedVersion = Prelude.Nothing,
      removeThingType = Prelude.Nothing,
      thingTypeName = Prelude.Nothing,
      thingName = pThingName_
    }

-- | A list of thing attributes, a JSON string containing name-value pairs.
-- For example:
--
-- @{\\\"attributes\\\":{\\\"name1\\\":\\\"value2\\\"}}@
--
-- This data is used to add new attributes or update existing attributes.
updateThing_attributePayload :: Lens.Lens' UpdateThing (Prelude.Maybe AttributePayload)
updateThing_attributePayload = Lens.lens (\UpdateThing' {attributePayload} -> attributePayload) (\s@UpdateThing' {} a -> s {attributePayload = a} :: UpdateThing)

-- | The expected version of the thing record in the registry. If the version
-- of the record in the registry does not match the expected version
-- specified in the request, the @UpdateThing@ request is rejected with a
-- @VersionConflictException@.
updateThing_expectedVersion :: Lens.Lens' UpdateThing (Prelude.Maybe Prelude.Integer)
updateThing_expectedVersion = Lens.lens (\UpdateThing' {expectedVersion} -> expectedVersion) (\s@UpdateThing' {} a -> s {expectedVersion = a} :: UpdateThing)

-- | Remove a thing type association. If __true__, the association is
-- removed.
updateThing_removeThingType :: Lens.Lens' UpdateThing (Prelude.Maybe Prelude.Bool)
updateThing_removeThingType = Lens.lens (\UpdateThing' {removeThingType} -> removeThingType) (\s@UpdateThing' {} a -> s {removeThingType = a} :: UpdateThing)

-- | The name of the thing type.
updateThing_thingTypeName :: Lens.Lens' UpdateThing (Prelude.Maybe Prelude.Text)
updateThing_thingTypeName = Lens.lens (\UpdateThing' {thingTypeName} -> thingTypeName) (\s@UpdateThing' {} a -> s {thingTypeName = a} :: UpdateThing)

-- | The name of the thing to update.
--
-- You can\'t change a thing\'s name. To change a thing\'s name, you must
-- create a new thing, give it the new name, and then delete the old thing.
updateThing_thingName :: Lens.Lens' UpdateThing Prelude.Text
updateThing_thingName = Lens.lens (\UpdateThing' {thingName} -> thingName) (\s@UpdateThing' {} a -> s {thingName = a} :: UpdateThing)

instance Core.AWSRequest UpdateThing where
  type AWSResponse UpdateThing = UpdateThingResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateThingResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateThing where
  hashWithSalt _salt UpdateThing' {..} =
    _salt
      `Prelude.hashWithSalt` attributePayload
      `Prelude.hashWithSalt` expectedVersion
      `Prelude.hashWithSalt` removeThingType
      `Prelude.hashWithSalt` thingTypeName
      `Prelude.hashWithSalt` thingName

instance Prelude.NFData UpdateThing where
  rnf UpdateThing' {..} =
    Prelude.rnf attributePayload
      `Prelude.seq` Prelude.rnf expectedVersion
      `Prelude.seq` Prelude.rnf removeThingType
      `Prelude.seq` Prelude.rnf thingTypeName
      `Prelude.seq` Prelude.rnf thingName

instance Data.ToHeaders UpdateThing where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdateThing where
  toJSON UpdateThing' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("attributePayload" Data..=)
              Prelude.<$> attributePayload,
            ("expectedVersion" Data..=)
              Prelude.<$> expectedVersion,
            ("removeThingType" Data..=)
              Prelude.<$> removeThingType,
            ("thingTypeName" Data..=) Prelude.<$> thingTypeName
          ]
      )

instance Data.ToPath UpdateThing where
  toPath UpdateThing' {..} =
    Prelude.mconcat ["/things/", Data.toBS thingName]

instance Data.ToQuery UpdateThing where
  toQuery = Prelude.const Prelude.mempty

-- | The output from the UpdateThing operation.
--
-- /See:/ 'newUpdateThingResponse' smart constructor.
data UpdateThingResponse = UpdateThingResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData UpdateThingResponse where
  rnf UpdateThingResponse' {..} = Prelude.rnf httpStatus
