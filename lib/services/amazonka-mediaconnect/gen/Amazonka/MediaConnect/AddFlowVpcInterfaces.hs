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
-- Module      : Amazonka.MediaConnect.AddFlowVpcInterfaces
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds VPC interfaces to flow
module Amazonka.MediaConnect.AddFlowVpcInterfaces
  ( -- * Creating a Request
    AddFlowVpcInterfaces (..),
    newAddFlowVpcInterfaces,

    -- * Request Lenses
    addFlowVpcInterfaces_flowArn,
    addFlowVpcInterfaces_vpcInterfaces,

    -- * Destructuring the Response
    AddFlowVpcInterfacesResponse (..),
    newAddFlowVpcInterfacesResponse,

    -- * Response Lenses
    addFlowVpcInterfacesResponse_flowArn,
    addFlowVpcInterfacesResponse_vpcInterfaces,
    addFlowVpcInterfacesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A request to add VPC interfaces to the flow.
--
-- /See:/ 'newAddFlowVpcInterfaces' smart constructor.
data AddFlowVpcInterfaces = AddFlowVpcInterfaces'
  { -- | The flow that you want to mutate.
    flowArn :: Prelude.Text,
    -- | A list of VPC interfaces that you want to add.
    vpcInterfaces :: [VpcInterfaceRequest]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddFlowVpcInterfaces' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flowArn', 'addFlowVpcInterfaces_flowArn' - The flow that you want to mutate.
--
-- 'vpcInterfaces', 'addFlowVpcInterfaces_vpcInterfaces' - A list of VPC interfaces that you want to add.
newAddFlowVpcInterfaces ::
  -- | 'flowArn'
  Prelude.Text ->
  AddFlowVpcInterfaces
newAddFlowVpcInterfaces pFlowArn_ =
  AddFlowVpcInterfaces'
    { flowArn = pFlowArn_,
      vpcInterfaces = Prelude.mempty
    }

-- | The flow that you want to mutate.
addFlowVpcInterfaces_flowArn :: Lens.Lens' AddFlowVpcInterfaces Prelude.Text
addFlowVpcInterfaces_flowArn = Lens.lens (\AddFlowVpcInterfaces' {flowArn} -> flowArn) (\s@AddFlowVpcInterfaces' {} a -> s {flowArn = a} :: AddFlowVpcInterfaces)

-- | A list of VPC interfaces that you want to add.
addFlowVpcInterfaces_vpcInterfaces :: Lens.Lens' AddFlowVpcInterfaces [VpcInterfaceRequest]
addFlowVpcInterfaces_vpcInterfaces = Lens.lens (\AddFlowVpcInterfaces' {vpcInterfaces} -> vpcInterfaces) (\s@AddFlowVpcInterfaces' {} a -> s {vpcInterfaces = a} :: AddFlowVpcInterfaces) Prelude.. Lens.coerced

instance Core.AWSRequest AddFlowVpcInterfaces where
  type
    AWSResponse AddFlowVpcInterfaces =
      AddFlowVpcInterfacesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AddFlowVpcInterfacesResponse'
            Prelude.<$> (x Data..?> "flowArn")
            Prelude.<*> (x Data..?> "vpcInterfaces" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AddFlowVpcInterfaces where
  hashWithSalt _salt AddFlowVpcInterfaces' {..} =
    _salt
      `Prelude.hashWithSalt` flowArn
      `Prelude.hashWithSalt` vpcInterfaces

instance Prelude.NFData AddFlowVpcInterfaces where
  rnf AddFlowVpcInterfaces' {..} =
    Prelude.rnf flowArn
      `Prelude.seq` Prelude.rnf vpcInterfaces

instance Data.ToHeaders AddFlowVpcInterfaces where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AddFlowVpcInterfaces where
  toJSON AddFlowVpcInterfaces' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("vpcInterfaces" Data..= vpcInterfaces)
          ]
      )

instance Data.ToPath AddFlowVpcInterfaces where
  toPath AddFlowVpcInterfaces' {..} =
    Prelude.mconcat
      ["/v1/flows/", Data.toBS flowArn, "/vpcInterfaces"]

instance Data.ToQuery AddFlowVpcInterfaces where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAddFlowVpcInterfacesResponse' smart constructor.
data AddFlowVpcInterfacesResponse = AddFlowVpcInterfacesResponse'
  { -- | The ARN of the flow that these VPC interfaces were added to.
    flowArn :: Prelude.Maybe Prelude.Text,
    -- | The details of the newly added VPC interfaces.
    vpcInterfaces :: Prelude.Maybe [VpcInterface],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddFlowVpcInterfacesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flowArn', 'addFlowVpcInterfacesResponse_flowArn' - The ARN of the flow that these VPC interfaces were added to.
--
-- 'vpcInterfaces', 'addFlowVpcInterfacesResponse_vpcInterfaces' - The details of the newly added VPC interfaces.
--
-- 'httpStatus', 'addFlowVpcInterfacesResponse_httpStatus' - The response's http status code.
newAddFlowVpcInterfacesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AddFlowVpcInterfacesResponse
newAddFlowVpcInterfacesResponse pHttpStatus_ =
  AddFlowVpcInterfacesResponse'
    { flowArn =
        Prelude.Nothing,
      vpcInterfaces = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the flow that these VPC interfaces were added to.
addFlowVpcInterfacesResponse_flowArn :: Lens.Lens' AddFlowVpcInterfacesResponse (Prelude.Maybe Prelude.Text)
addFlowVpcInterfacesResponse_flowArn = Lens.lens (\AddFlowVpcInterfacesResponse' {flowArn} -> flowArn) (\s@AddFlowVpcInterfacesResponse' {} a -> s {flowArn = a} :: AddFlowVpcInterfacesResponse)

-- | The details of the newly added VPC interfaces.
addFlowVpcInterfacesResponse_vpcInterfaces :: Lens.Lens' AddFlowVpcInterfacesResponse (Prelude.Maybe [VpcInterface])
addFlowVpcInterfacesResponse_vpcInterfaces = Lens.lens (\AddFlowVpcInterfacesResponse' {vpcInterfaces} -> vpcInterfaces) (\s@AddFlowVpcInterfacesResponse' {} a -> s {vpcInterfaces = a} :: AddFlowVpcInterfacesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
addFlowVpcInterfacesResponse_httpStatus :: Lens.Lens' AddFlowVpcInterfacesResponse Prelude.Int
addFlowVpcInterfacesResponse_httpStatus = Lens.lens (\AddFlowVpcInterfacesResponse' {httpStatus} -> httpStatus) (\s@AddFlowVpcInterfacesResponse' {} a -> s {httpStatus = a} :: AddFlowVpcInterfacesResponse)

instance Prelude.NFData AddFlowVpcInterfacesResponse where
  rnf AddFlowVpcInterfacesResponse' {..} =
    Prelude.rnf flowArn
      `Prelude.seq` Prelude.rnf vpcInterfaces
      `Prelude.seq` Prelude.rnf httpStatus
