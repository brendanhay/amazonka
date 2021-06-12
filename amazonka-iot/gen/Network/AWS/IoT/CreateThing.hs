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
-- Module      : Network.AWS.IoT.CreateThing
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.IoT.CreateThing
  ( -- * Creating a Request
    CreateThing (..),
    newCreateThing,

    -- * Request Lenses
    createThing_billingGroupName,
    createThing_thingTypeName,
    createThing_attributePayload,
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

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the CreateThing operation.
--
-- /See:/ 'newCreateThing' smart constructor.
data CreateThing = CreateThing'
  { -- | The name of the billing group the thing will be added to.
    billingGroupName :: Core.Maybe Core.Text,
    -- | The name of the thing type associated with the new thing.
    thingTypeName :: Core.Maybe Core.Text,
    -- | The attribute payload, which consists of up to three name\/value pairs
    -- in a JSON document. For example:
    --
    -- @{\\\"attributes\\\":{\\\"string1\\\":\\\"string2\\\"}}@
    attributePayload :: Core.Maybe AttributePayload,
    -- | The name of the thing to create.
    --
    -- You can\'t change a thing\'s name after you create it. To change a
    -- thing\'s name, you must create a new thing, give it the new name, and
    -- then delete the old thing.
    thingName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateThing' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'billingGroupName', 'createThing_billingGroupName' - The name of the billing group the thing will be added to.
--
-- 'thingTypeName', 'createThing_thingTypeName' - The name of the thing type associated with the new thing.
--
-- 'attributePayload', 'createThing_attributePayload' - The attribute payload, which consists of up to three name\/value pairs
-- in a JSON document. For example:
--
-- @{\\\"attributes\\\":{\\\"string1\\\":\\\"string2\\\"}}@
--
-- 'thingName', 'createThing_thingName' - The name of the thing to create.
--
-- You can\'t change a thing\'s name after you create it. To change a
-- thing\'s name, you must create a new thing, give it the new name, and
-- then delete the old thing.
newCreateThing ::
  -- | 'thingName'
  Core.Text ->
  CreateThing
newCreateThing pThingName_ =
  CreateThing'
    { billingGroupName = Core.Nothing,
      thingTypeName = Core.Nothing,
      attributePayload = Core.Nothing,
      thingName = pThingName_
    }

-- | The name of the billing group the thing will be added to.
createThing_billingGroupName :: Lens.Lens' CreateThing (Core.Maybe Core.Text)
createThing_billingGroupName = Lens.lens (\CreateThing' {billingGroupName} -> billingGroupName) (\s@CreateThing' {} a -> s {billingGroupName = a} :: CreateThing)

-- | The name of the thing type associated with the new thing.
createThing_thingTypeName :: Lens.Lens' CreateThing (Core.Maybe Core.Text)
createThing_thingTypeName = Lens.lens (\CreateThing' {thingTypeName} -> thingTypeName) (\s@CreateThing' {} a -> s {thingTypeName = a} :: CreateThing)

-- | The attribute payload, which consists of up to three name\/value pairs
-- in a JSON document. For example:
--
-- @{\\\"attributes\\\":{\\\"string1\\\":\\\"string2\\\"}}@
createThing_attributePayload :: Lens.Lens' CreateThing (Core.Maybe AttributePayload)
createThing_attributePayload = Lens.lens (\CreateThing' {attributePayload} -> attributePayload) (\s@CreateThing' {} a -> s {attributePayload = a} :: CreateThing)

-- | The name of the thing to create.
--
-- You can\'t change a thing\'s name after you create it. To change a
-- thing\'s name, you must create a new thing, give it the new name, and
-- then delete the old thing.
createThing_thingName :: Lens.Lens' CreateThing Core.Text
createThing_thingName = Lens.lens (\CreateThing' {thingName} -> thingName) (\s@CreateThing' {} a -> s {thingName = a} :: CreateThing)

instance Core.AWSRequest CreateThing where
  type AWSResponse CreateThing = CreateThingResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateThingResponse'
            Core.<$> (x Core..?> "thingArn")
            Core.<*> (x Core..?> "thingId")
            Core.<*> (x Core..?> "thingName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateThing

instance Core.NFData CreateThing

instance Core.ToHeaders CreateThing where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON CreateThing where
  toJSON CreateThing' {..} =
    Core.object
      ( Core.catMaybes
          [ ("billingGroupName" Core..=)
              Core.<$> billingGroupName,
            ("thingTypeName" Core..=) Core.<$> thingTypeName,
            ("attributePayload" Core..=)
              Core.<$> attributePayload
          ]
      )

instance Core.ToPath CreateThing where
  toPath CreateThing' {..} =
    Core.mconcat ["/things/", Core.toBS thingName]

instance Core.ToQuery CreateThing where
  toQuery = Core.const Core.mempty

-- | The output of the CreateThing operation.
--
-- /See:/ 'newCreateThingResponse' smart constructor.
data CreateThingResponse = CreateThingResponse'
  { -- | The ARN of the new thing.
    thingArn :: Core.Maybe Core.Text,
    -- | The thing ID.
    thingId :: Core.Maybe Core.Text,
    -- | The name of the new thing.
    thingName :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  CreateThingResponse
newCreateThingResponse pHttpStatus_ =
  CreateThingResponse'
    { thingArn = Core.Nothing,
      thingId = Core.Nothing,
      thingName = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the new thing.
createThingResponse_thingArn :: Lens.Lens' CreateThingResponse (Core.Maybe Core.Text)
createThingResponse_thingArn = Lens.lens (\CreateThingResponse' {thingArn} -> thingArn) (\s@CreateThingResponse' {} a -> s {thingArn = a} :: CreateThingResponse)

-- | The thing ID.
createThingResponse_thingId :: Lens.Lens' CreateThingResponse (Core.Maybe Core.Text)
createThingResponse_thingId = Lens.lens (\CreateThingResponse' {thingId} -> thingId) (\s@CreateThingResponse' {} a -> s {thingId = a} :: CreateThingResponse)

-- | The name of the new thing.
createThingResponse_thingName :: Lens.Lens' CreateThingResponse (Core.Maybe Core.Text)
createThingResponse_thingName = Lens.lens (\CreateThingResponse' {thingName} -> thingName) (\s@CreateThingResponse' {} a -> s {thingName = a} :: CreateThingResponse)

-- | The response's http status code.
createThingResponse_httpStatus :: Lens.Lens' CreateThingResponse Core.Int
createThingResponse_httpStatus = Lens.lens (\CreateThingResponse' {httpStatus} -> httpStatus) (\s@CreateThingResponse' {} a -> s {httpStatus = a} :: CreateThingResponse)

instance Core.NFData CreateThingResponse
