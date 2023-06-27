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
-- Module      : Amazonka.IoT.CreateThing
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a thing record in the registry. If this call is made multiple
-- times using the same thing name and configuration, the call will
-- succeed. If this call is made with the same thing name but different
-- configuration a @ResourceAlreadyExistsException@ is thrown.
--
-- This is a control plane operation. See
-- <https://docs.aws.amazon.com/iot/latest/developerguide/iot-authorization.html Authorization>
-- for information about authorizing control plane actions.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions CreateThing>
-- action.
module Amazonka.IoT.CreateThing
  ( -- * Creating a Request
    CreateThing (..),
    newCreateThing,

    -- * Request Lenses
    createThing_attributePayload,
    createThing_billingGroupName,
    createThing_thingTypeName,
    createThing_thingName,

    -- * Destructuring the Response
    CreateThingResponse (..),
    newCreateThingResponse,

    -- * Response Lenses
    createThingResponse_thingArn,
    createThingResponse_thingId,
    createThingResponse_thingName,
    createThingResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input for the CreateThing operation.
--
-- /See:/ 'newCreateThing' smart constructor.
data CreateThing = CreateThing'
  { -- | The attribute payload, which consists of up to three name\/value pairs
    -- in a JSON document. For example:
    --
    -- @{\\\"attributes\\\":{\\\"string1\\\":\\\"string2\\\"}}@
    attributePayload :: Prelude.Maybe AttributePayload,
    -- | The name of the billing group the thing will be added to.
    billingGroupName :: Prelude.Maybe Prelude.Text,
    -- | The name of the thing type associated with the new thing.
    thingTypeName :: Prelude.Maybe Prelude.Text,
    -- | The name of the thing to create.
    --
    -- You can\'t change a thing\'s name after you create it. To change a
    -- thing\'s name, you must create a new thing, give it the new name, and
    -- then delete the old thing.
    thingName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateThing' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributePayload', 'createThing_attributePayload' - The attribute payload, which consists of up to three name\/value pairs
-- in a JSON document. For example:
--
-- @{\\\"attributes\\\":{\\\"string1\\\":\\\"string2\\\"}}@
--
-- 'billingGroupName', 'createThing_billingGroupName' - The name of the billing group the thing will be added to.
--
-- 'thingTypeName', 'createThing_thingTypeName' - The name of the thing type associated with the new thing.
--
-- 'thingName', 'createThing_thingName' - The name of the thing to create.
--
-- You can\'t change a thing\'s name after you create it. To change a
-- thing\'s name, you must create a new thing, give it the new name, and
-- then delete the old thing.
newCreateThing ::
  -- | 'thingName'
  Prelude.Text ->
  CreateThing
newCreateThing pThingName_ =
  CreateThing'
    { attributePayload = Prelude.Nothing,
      billingGroupName = Prelude.Nothing,
      thingTypeName = Prelude.Nothing,
      thingName = pThingName_
    }

-- | The attribute payload, which consists of up to three name\/value pairs
-- in a JSON document. For example:
--
-- @{\\\"attributes\\\":{\\\"string1\\\":\\\"string2\\\"}}@
createThing_attributePayload :: Lens.Lens' CreateThing (Prelude.Maybe AttributePayload)
createThing_attributePayload = Lens.lens (\CreateThing' {attributePayload} -> attributePayload) (\s@CreateThing' {} a -> s {attributePayload = a} :: CreateThing)

-- | The name of the billing group the thing will be added to.
createThing_billingGroupName :: Lens.Lens' CreateThing (Prelude.Maybe Prelude.Text)
createThing_billingGroupName = Lens.lens (\CreateThing' {billingGroupName} -> billingGroupName) (\s@CreateThing' {} a -> s {billingGroupName = a} :: CreateThing)

-- | The name of the thing type associated with the new thing.
createThing_thingTypeName :: Lens.Lens' CreateThing (Prelude.Maybe Prelude.Text)
createThing_thingTypeName = Lens.lens (\CreateThing' {thingTypeName} -> thingTypeName) (\s@CreateThing' {} a -> s {thingTypeName = a} :: CreateThing)

-- | The name of the thing to create.
--
-- You can\'t change a thing\'s name after you create it. To change a
-- thing\'s name, you must create a new thing, give it the new name, and
-- then delete the old thing.
createThing_thingName :: Lens.Lens' CreateThing Prelude.Text
createThing_thingName = Lens.lens (\CreateThing' {thingName} -> thingName) (\s@CreateThing' {} a -> s {thingName = a} :: CreateThing)

instance Core.AWSRequest CreateThing where
  type AWSResponse CreateThing = CreateThingResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateThingResponse'
            Prelude.<$> (x Data..?> "thingArn")
            Prelude.<*> (x Data..?> "thingId")
            Prelude.<*> (x Data..?> "thingName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateThing where
  hashWithSalt _salt CreateThing' {..} =
    _salt
      `Prelude.hashWithSalt` attributePayload
      `Prelude.hashWithSalt` billingGroupName
      `Prelude.hashWithSalt` thingTypeName
      `Prelude.hashWithSalt` thingName

instance Prelude.NFData CreateThing where
  rnf CreateThing' {..} =
    Prelude.rnf attributePayload
      `Prelude.seq` Prelude.rnf billingGroupName
      `Prelude.seq` Prelude.rnf thingTypeName
      `Prelude.seq` Prelude.rnf thingName

instance Data.ToHeaders CreateThing where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreateThing where
  toJSON CreateThing' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("attributePayload" Data..=)
              Prelude.<$> attributePayload,
            ("billingGroupName" Data..=)
              Prelude.<$> billingGroupName,
            ("thingTypeName" Data..=) Prelude.<$> thingTypeName
          ]
      )

instance Data.ToPath CreateThing where
  toPath CreateThing' {..} =
    Prelude.mconcat ["/things/", Data.toBS thingName]

instance Data.ToQuery CreateThing where
  toQuery = Prelude.const Prelude.mempty

-- | The output of the CreateThing operation.
--
-- /See:/ 'newCreateThingResponse' smart constructor.
data CreateThingResponse = CreateThingResponse'
  { -- | The ARN of the new thing.
    thingArn :: Prelude.Maybe Prelude.Text,
    -- | The thing ID.
    thingId :: Prelude.Maybe Prelude.Text,
    -- | The name of the new thing.
    thingName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateThingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'thingArn', 'createThingResponse_thingArn' - The ARN of the new thing.
--
-- 'thingId', 'createThingResponse_thingId' - The thing ID.
--
-- 'thingName', 'createThingResponse_thingName' - The name of the new thing.
--
-- 'httpStatus', 'createThingResponse_httpStatus' - The response's http status code.
newCreateThingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateThingResponse
newCreateThingResponse pHttpStatus_ =
  CreateThingResponse'
    { thingArn = Prelude.Nothing,
      thingId = Prelude.Nothing,
      thingName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the new thing.
createThingResponse_thingArn :: Lens.Lens' CreateThingResponse (Prelude.Maybe Prelude.Text)
createThingResponse_thingArn = Lens.lens (\CreateThingResponse' {thingArn} -> thingArn) (\s@CreateThingResponse' {} a -> s {thingArn = a} :: CreateThingResponse)

-- | The thing ID.
createThingResponse_thingId :: Lens.Lens' CreateThingResponse (Prelude.Maybe Prelude.Text)
createThingResponse_thingId = Lens.lens (\CreateThingResponse' {thingId} -> thingId) (\s@CreateThingResponse' {} a -> s {thingId = a} :: CreateThingResponse)

-- | The name of the new thing.
createThingResponse_thingName :: Lens.Lens' CreateThingResponse (Prelude.Maybe Prelude.Text)
createThingResponse_thingName = Lens.lens (\CreateThingResponse' {thingName} -> thingName) (\s@CreateThingResponse' {} a -> s {thingName = a} :: CreateThingResponse)

-- | The response's http status code.
createThingResponse_httpStatus :: Lens.Lens' CreateThingResponse Prelude.Int
createThingResponse_httpStatus = Lens.lens (\CreateThingResponse' {httpStatus} -> httpStatus) (\s@CreateThingResponse' {} a -> s {httpStatus = a} :: CreateThingResponse)

instance Prelude.NFData CreateThingResponse where
  rnf CreateThingResponse' {..} =
    Prelude.rnf thingArn
      `Prelude.seq` Prelude.rnf thingId
      `Prelude.seq` Prelude.rnf thingName
      `Prelude.seq` Prelude.rnf httpStatus
