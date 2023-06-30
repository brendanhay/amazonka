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
-- Module      : Amazonka.Connect.DescribeContactFlowModule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified flow module.
module Amazonka.Connect.DescribeContactFlowModule
  ( -- * Creating a Request
    DescribeContactFlowModule (..),
    newDescribeContactFlowModule,

    -- * Request Lenses
    describeContactFlowModule_instanceId,
    describeContactFlowModule_contactFlowModuleId,

    -- * Destructuring the Response
    DescribeContactFlowModuleResponse (..),
    newDescribeContactFlowModuleResponse,

    -- * Response Lenses
    describeContactFlowModuleResponse_contactFlowModule,
    describeContactFlowModuleResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeContactFlowModule' smart constructor.
data DescribeContactFlowModule = DescribeContactFlowModule'
  { -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The identifier of the flow module.
    contactFlowModuleId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeContactFlowModule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'describeContactFlowModule_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'contactFlowModuleId', 'describeContactFlowModule_contactFlowModuleId' - The identifier of the flow module.
newDescribeContactFlowModule ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'contactFlowModuleId'
  Prelude.Text ->
  DescribeContactFlowModule
newDescribeContactFlowModule
  pInstanceId_
  pContactFlowModuleId_ =
    DescribeContactFlowModule'
      { instanceId =
          pInstanceId_,
        contactFlowModuleId = pContactFlowModuleId_
      }

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
describeContactFlowModule_instanceId :: Lens.Lens' DescribeContactFlowModule Prelude.Text
describeContactFlowModule_instanceId = Lens.lens (\DescribeContactFlowModule' {instanceId} -> instanceId) (\s@DescribeContactFlowModule' {} a -> s {instanceId = a} :: DescribeContactFlowModule)

-- | The identifier of the flow module.
describeContactFlowModule_contactFlowModuleId :: Lens.Lens' DescribeContactFlowModule Prelude.Text
describeContactFlowModule_contactFlowModuleId = Lens.lens (\DescribeContactFlowModule' {contactFlowModuleId} -> contactFlowModuleId) (\s@DescribeContactFlowModule' {} a -> s {contactFlowModuleId = a} :: DescribeContactFlowModule)

instance Core.AWSRequest DescribeContactFlowModule where
  type
    AWSResponse DescribeContactFlowModule =
      DescribeContactFlowModuleResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeContactFlowModuleResponse'
            Prelude.<$> (x Data..?> "ContactFlowModule")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeContactFlowModule where
  hashWithSalt _salt DescribeContactFlowModule' {..} =
    _salt
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` contactFlowModuleId

instance Prelude.NFData DescribeContactFlowModule where
  rnf DescribeContactFlowModule' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf contactFlowModuleId

instance Data.ToHeaders DescribeContactFlowModule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeContactFlowModule where
  toPath DescribeContactFlowModule' {..} =
    Prelude.mconcat
      [ "/contact-flow-modules/",
        Data.toBS instanceId,
        "/",
        Data.toBS contactFlowModuleId
      ]

instance Data.ToQuery DescribeContactFlowModule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeContactFlowModuleResponse' smart constructor.
data DescribeContactFlowModuleResponse = DescribeContactFlowModuleResponse'
  { -- | Information about the flow module.
    contactFlowModule :: Prelude.Maybe ContactFlowModule,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeContactFlowModuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contactFlowModule', 'describeContactFlowModuleResponse_contactFlowModule' - Information about the flow module.
--
-- 'httpStatus', 'describeContactFlowModuleResponse_httpStatus' - The response's http status code.
newDescribeContactFlowModuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeContactFlowModuleResponse
newDescribeContactFlowModuleResponse pHttpStatus_ =
  DescribeContactFlowModuleResponse'
    { contactFlowModule =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the flow module.
describeContactFlowModuleResponse_contactFlowModule :: Lens.Lens' DescribeContactFlowModuleResponse (Prelude.Maybe ContactFlowModule)
describeContactFlowModuleResponse_contactFlowModule = Lens.lens (\DescribeContactFlowModuleResponse' {contactFlowModule} -> contactFlowModule) (\s@DescribeContactFlowModuleResponse' {} a -> s {contactFlowModule = a} :: DescribeContactFlowModuleResponse)

-- | The response's http status code.
describeContactFlowModuleResponse_httpStatus :: Lens.Lens' DescribeContactFlowModuleResponse Prelude.Int
describeContactFlowModuleResponse_httpStatus = Lens.lens (\DescribeContactFlowModuleResponse' {httpStatus} -> httpStatus) (\s@DescribeContactFlowModuleResponse' {} a -> s {httpStatus = a} :: DescribeContactFlowModuleResponse)

instance
  Prelude.NFData
    DescribeContactFlowModuleResponse
  where
  rnf DescribeContactFlowModuleResponse' {..} =
    Prelude.rnf contactFlowModule
      `Prelude.seq` Prelude.rnf httpStatus
