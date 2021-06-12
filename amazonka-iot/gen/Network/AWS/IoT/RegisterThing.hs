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
-- Module      : Network.AWS.IoT.RegisterThing
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provisions a thing in the device registry. RegisterThing calls other AWS
-- IoT control plane APIs. These calls might exceed your account level
-- <https://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits_iot AWS IoT Throttling Limits>
-- and cause throttle errors. Please contact
-- <https://console.aws.amazon.com/support/home AWS Customer Support> to
-- raise your throttling limits if necessary.
module Network.AWS.IoT.RegisterThing
  ( -- * Creating a Request
    RegisterThing (..),
    newRegisterThing,

    -- * Request Lenses
    registerThing_parameters,
    registerThing_templateBody,

    -- * Destructuring the Response
    RegisterThingResponse (..),
    newRegisterThingResponse,

    -- * Response Lenses
    registerThingResponse_resourceArns,
    registerThingResponse_certificatePem,
    registerThingResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRegisterThing' smart constructor.
data RegisterThing = RegisterThing'
  { -- | The parameters for provisioning a thing. See
    -- <https://docs.aws.amazon.com/iot/latest/developerguide/provision-template.html Provisioning Templates>
    -- for more information.
    parameters :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The provisioning template. See
    -- <https://docs.aws.amazon.com/iot/latest/developerguide/provision-w-cert.html Provisioning Devices That Have Device Certificates>
    -- for more information.
    templateBody :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RegisterThing' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameters', 'registerThing_parameters' - The parameters for provisioning a thing. See
-- <https://docs.aws.amazon.com/iot/latest/developerguide/provision-template.html Provisioning Templates>
-- for more information.
--
-- 'templateBody', 'registerThing_templateBody' - The provisioning template. See
-- <https://docs.aws.amazon.com/iot/latest/developerguide/provision-w-cert.html Provisioning Devices That Have Device Certificates>
-- for more information.
newRegisterThing ::
  -- | 'templateBody'
  Core.Text ->
  RegisterThing
newRegisterThing pTemplateBody_ =
  RegisterThing'
    { parameters = Core.Nothing,
      templateBody = pTemplateBody_
    }

-- | The parameters for provisioning a thing. See
-- <https://docs.aws.amazon.com/iot/latest/developerguide/provision-template.html Provisioning Templates>
-- for more information.
registerThing_parameters :: Lens.Lens' RegisterThing (Core.Maybe (Core.HashMap Core.Text Core.Text))
registerThing_parameters = Lens.lens (\RegisterThing' {parameters} -> parameters) (\s@RegisterThing' {} a -> s {parameters = a} :: RegisterThing) Core.. Lens.mapping Lens._Coerce

-- | The provisioning template. See
-- <https://docs.aws.amazon.com/iot/latest/developerguide/provision-w-cert.html Provisioning Devices That Have Device Certificates>
-- for more information.
registerThing_templateBody :: Lens.Lens' RegisterThing Core.Text
registerThing_templateBody = Lens.lens (\RegisterThing' {templateBody} -> templateBody) (\s@RegisterThing' {} a -> s {templateBody = a} :: RegisterThing)

instance Core.AWSRequest RegisterThing where
  type
    AWSResponse RegisterThing =
      RegisterThingResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterThingResponse'
            Core.<$> (x Core..?> "resourceArns" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "certificatePem")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable RegisterThing

instance Core.NFData RegisterThing

instance Core.ToHeaders RegisterThing where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON RegisterThing where
  toJSON RegisterThing' {..} =
    Core.object
      ( Core.catMaybes
          [ ("parameters" Core..=) Core.<$> parameters,
            Core.Just ("templateBody" Core..= templateBody)
          ]
      )

instance Core.ToPath RegisterThing where
  toPath = Core.const "/things"

instance Core.ToQuery RegisterThing where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newRegisterThingResponse' smart constructor.
data RegisterThingResponse = RegisterThingResponse'
  { -- | ARNs for the generated resources.
    resourceArns :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The certificate data, in PEM format.
    certificatePem :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RegisterThingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArns', 'registerThingResponse_resourceArns' - ARNs for the generated resources.
--
-- 'certificatePem', 'registerThingResponse_certificatePem' - The certificate data, in PEM format.
--
-- 'httpStatus', 'registerThingResponse_httpStatus' - The response's http status code.
newRegisterThingResponse ::
  -- | 'httpStatus'
  Core.Int ->
  RegisterThingResponse
newRegisterThingResponse pHttpStatus_ =
  RegisterThingResponse'
    { resourceArns = Core.Nothing,
      certificatePem = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | ARNs for the generated resources.
registerThingResponse_resourceArns :: Lens.Lens' RegisterThingResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
registerThingResponse_resourceArns = Lens.lens (\RegisterThingResponse' {resourceArns} -> resourceArns) (\s@RegisterThingResponse' {} a -> s {resourceArns = a} :: RegisterThingResponse) Core.. Lens.mapping Lens._Coerce

-- | The certificate data, in PEM format.
registerThingResponse_certificatePem :: Lens.Lens' RegisterThingResponse (Core.Maybe Core.Text)
registerThingResponse_certificatePem = Lens.lens (\RegisterThingResponse' {certificatePem} -> certificatePem) (\s@RegisterThingResponse' {} a -> s {certificatePem = a} :: RegisterThingResponse)

-- | The response's http status code.
registerThingResponse_httpStatus :: Lens.Lens' RegisterThingResponse Core.Int
registerThingResponse_httpStatus = Lens.lens (\RegisterThingResponse' {httpStatus} -> httpStatus) (\s@RegisterThingResponse' {} a -> s {httpStatus = a} :: RegisterThingResponse)

instance Core.NFData RegisterThingResponse
