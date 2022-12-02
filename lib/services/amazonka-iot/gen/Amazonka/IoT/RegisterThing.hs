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
-- Module      : Amazonka.IoT.RegisterThing
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provisions a thing in the device registry. RegisterThing calls other IoT
-- control plane APIs. These calls might exceed your account level
-- <https://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits_iot IoT Throttling Limits>
-- and cause throttle errors. Please contact
-- <https://console.aws.amazon.com/support/home Amazon Web Services Customer Support>
-- to raise your throttling limits if necessary.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions RegisterThing>
-- action.
module Amazonka.IoT.RegisterThing
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
    registerThingResponse_certificatePem,
    registerThingResponse_resourceArns,
    registerThingResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRegisterThing' smart constructor.
data RegisterThing = RegisterThing'
  { -- | The parameters for provisioning a thing. See
    -- <https://docs.aws.amazon.com/iot/latest/developerguide/provision-template.html Provisioning Templates>
    -- for more information.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The provisioning template. See
    -- <https://docs.aws.amazon.com/iot/latest/developerguide/provision-w-cert.html Provisioning Devices That Have Device Certificates>
    -- for more information.
    templateBody :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  RegisterThing
newRegisterThing pTemplateBody_ =
  RegisterThing'
    { parameters = Prelude.Nothing,
      templateBody = pTemplateBody_
    }

-- | The parameters for provisioning a thing. See
-- <https://docs.aws.amazon.com/iot/latest/developerguide/provision-template.html Provisioning Templates>
-- for more information.
registerThing_parameters :: Lens.Lens' RegisterThing (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
registerThing_parameters = Lens.lens (\RegisterThing' {parameters} -> parameters) (\s@RegisterThing' {} a -> s {parameters = a} :: RegisterThing) Prelude.. Lens.mapping Lens.coerced

-- | The provisioning template. See
-- <https://docs.aws.amazon.com/iot/latest/developerguide/provision-w-cert.html Provisioning Devices That Have Device Certificates>
-- for more information.
registerThing_templateBody :: Lens.Lens' RegisterThing Prelude.Text
registerThing_templateBody = Lens.lens (\RegisterThing' {templateBody} -> templateBody) (\s@RegisterThing' {} a -> s {templateBody = a} :: RegisterThing)

instance Core.AWSRequest RegisterThing where
  type
    AWSResponse RegisterThing =
      RegisterThingResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterThingResponse'
            Prelude.<$> (x Data..?> "certificatePem")
            Prelude.<*> (x Data..?> "resourceArns" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RegisterThing where
  hashWithSalt _salt RegisterThing' {..} =
    _salt `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` templateBody

instance Prelude.NFData RegisterThing where
  rnf RegisterThing' {..} =
    Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf templateBody

instance Data.ToHeaders RegisterThing where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON RegisterThing where
  toJSON RegisterThing' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("parameters" Data..=) Prelude.<$> parameters,
            Prelude.Just ("templateBody" Data..= templateBody)
          ]
      )

instance Data.ToPath RegisterThing where
  toPath = Prelude.const "/things"

instance Data.ToQuery RegisterThing where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRegisterThingResponse' smart constructor.
data RegisterThingResponse = RegisterThingResponse'
  { -- | The certificate data, in PEM format.
    certificatePem :: Prelude.Maybe Prelude.Text,
    -- | ARNs for the generated resources.
    resourceArns :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterThingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificatePem', 'registerThingResponse_certificatePem' - The certificate data, in PEM format.
--
-- 'resourceArns', 'registerThingResponse_resourceArns' - ARNs for the generated resources.
--
-- 'httpStatus', 'registerThingResponse_httpStatus' - The response's http status code.
newRegisterThingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RegisterThingResponse
newRegisterThingResponse pHttpStatus_ =
  RegisterThingResponse'
    { certificatePem =
        Prelude.Nothing,
      resourceArns = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The certificate data, in PEM format.
registerThingResponse_certificatePem :: Lens.Lens' RegisterThingResponse (Prelude.Maybe Prelude.Text)
registerThingResponse_certificatePem = Lens.lens (\RegisterThingResponse' {certificatePem} -> certificatePem) (\s@RegisterThingResponse' {} a -> s {certificatePem = a} :: RegisterThingResponse)

-- | ARNs for the generated resources.
registerThingResponse_resourceArns :: Lens.Lens' RegisterThingResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
registerThingResponse_resourceArns = Lens.lens (\RegisterThingResponse' {resourceArns} -> resourceArns) (\s@RegisterThingResponse' {} a -> s {resourceArns = a} :: RegisterThingResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
registerThingResponse_httpStatus :: Lens.Lens' RegisterThingResponse Prelude.Int
registerThingResponse_httpStatus = Lens.lens (\RegisterThingResponse' {httpStatus} -> httpStatus) (\s@RegisterThingResponse' {} a -> s {httpStatus = a} :: RegisterThingResponse)

instance Prelude.NFData RegisterThingResponse where
  rnf RegisterThingResponse' {..} =
    Prelude.rnf certificatePem
      `Prelude.seq` Prelude.rnf resourceArns
      `Prelude.seq` Prelude.rnf httpStatus
