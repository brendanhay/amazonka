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
-- Module      : Amazonka.MediaConnect.RemoveFlowVpcInterface
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a VPC Interface from an existing flow. This request can be made
-- only on a VPC interface that does not have a Source or Output associated
-- with it. If the VPC interface is referenced by a Source or Output, you
-- must first delete or update the Source or Output to no longer reference
-- the VPC interface.
module Amazonka.MediaConnect.RemoveFlowVpcInterface
  ( -- * Creating a Request
    RemoveFlowVpcInterface (..),
    newRemoveFlowVpcInterface,

    -- * Request Lenses
    removeFlowVpcInterface_flowArn,
    removeFlowVpcInterface_vpcInterfaceName,

    -- * Destructuring the Response
    RemoveFlowVpcInterfaceResponse (..),
    newRemoveFlowVpcInterfaceResponse,

    -- * Response Lenses
    removeFlowVpcInterfaceResponse_flowArn,
    removeFlowVpcInterfaceResponse_nonDeletedNetworkInterfaceIds,
    removeFlowVpcInterfaceResponse_vpcInterfaceName,
    removeFlowVpcInterfaceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRemoveFlowVpcInterface' smart constructor.
data RemoveFlowVpcInterface = RemoveFlowVpcInterface'
  { -- | The flow that you want to remove a VPC interface from.
    flowArn :: Prelude.Text,
    -- | The name of the VPC interface that you want to remove.
    vpcInterfaceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveFlowVpcInterface' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flowArn', 'removeFlowVpcInterface_flowArn' - The flow that you want to remove a VPC interface from.
--
-- 'vpcInterfaceName', 'removeFlowVpcInterface_vpcInterfaceName' - The name of the VPC interface that you want to remove.
newRemoveFlowVpcInterface ::
  -- | 'flowArn'
  Prelude.Text ->
  -- | 'vpcInterfaceName'
  Prelude.Text ->
  RemoveFlowVpcInterface
newRemoveFlowVpcInterface
  pFlowArn_
  pVpcInterfaceName_ =
    RemoveFlowVpcInterface'
      { flowArn = pFlowArn_,
        vpcInterfaceName = pVpcInterfaceName_
      }

-- | The flow that you want to remove a VPC interface from.
removeFlowVpcInterface_flowArn :: Lens.Lens' RemoveFlowVpcInterface Prelude.Text
removeFlowVpcInterface_flowArn = Lens.lens (\RemoveFlowVpcInterface' {flowArn} -> flowArn) (\s@RemoveFlowVpcInterface' {} a -> s {flowArn = a} :: RemoveFlowVpcInterface)

-- | The name of the VPC interface that you want to remove.
removeFlowVpcInterface_vpcInterfaceName :: Lens.Lens' RemoveFlowVpcInterface Prelude.Text
removeFlowVpcInterface_vpcInterfaceName = Lens.lens (\RemoveFlowVpcInterface' {vpcInterfaceName} -> vpcInterfaceName) (\s@RemoveFlowVpcInterface' {} a -> s {vpcInterfaceName = a} :: RemoveFlowVpcInterface)

instance Core.AWSRequest RemoveFlowVpcInterface where
  type
    AWSResponse RemoveFlowVpcInterface =
      RemoveFlowVpcInterfaceResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RemoveFlowVpcInterfaceResponse'
            Prelude.<$> (x Data..?> "flowArn")
            Prelude.<*> ( x
                            Data..?> "nonDeletedNetworkInterfaceIds"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "vpcInterfaceName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RemoveFlowVpcInterface where
  hashWithSalt _salt RemoveFlowVpcInterface' {..} =
    _salt
      `Prelude.hashWithSalt` flowArn
      `Prelude.hashWithSalt` vpcInterfaceName

instance Prelude.NFData RemoveFlowVpcInterface where
  rnf RemoveFlowVpcInterface' {..} =
    Prelude.rnf flowArn
      `Prelude.seq` Prelude.rnf vpcInterfaceName

instance Data.ToHeaders RemoveFlowVpcInterface where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath RemoveFlowVpcInterface where
  toPath RemoveFlowVpcInterface' {..} =
    Prelude.mconcat
      [ "/v1/flows/",
        Data.toBS flowArn,
        "/vpcInterfaces/",
        Data.toBS vpcInterfaceName
      ]

instance Data.ToQuery RemoveFlowVpcInterface where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRemoveFlowVpcInterfaceResponse' smart constructor.
data RemoveFlowVpcInterfaceResponse = RemoveFlowVpcInterfaceResponse'
  { -- | The ARN of the flow that is associated with the VPC interface you
    -- removed.
    flowArn :: Prelude.Maybe Prelude.Text,
    -- | IDs of network interfaces associated with the removed VPC interface that
    -- Media Connect was unable to remove.
    nonDeletedNetworkInterfaceIds :: Prelude.Maybe [Prelude.Text],
    -- | The name of the VPC interface that was removed.
    vpcInterfaceName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveFlowVpcInterfaceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flowArn', 'removeFlowVpcInterfaceResponse_flowArn' - The ARN of the flow that is associated with the VPC interface you
-- removed.
--
-- 'nonDeletedNetworkInterfaceIds', 'removeFlowVpcInterfaceResponse_nonDeletedNetworkInterfaceIds' - IDs of network interfaces associated with the removed VPC interface that
-- Media Connect was unable to remove.
--
-- 'vpcInterfaceName', 'removeFlowVpcInterfaceResponse_vpcInterfaceName' - The name of the VPC interface that was removed.
--
-- 'httpStatus', 'removeFlowVpcInterfaceResponse_httpStatus' - The response's http status code.
newRemoveFlowVpcInterfaceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RemoveFlowVpcInterfaceResponse
newRemoveFlowVpcInterfaceResponse pHttpStatus_ =
  RemoveFlowVpcInterfaceResponse'
    { flowArn =
        Prelude.Nothing,
      nonDeletedNetworkInterfaceIds =
        Prelude.Nothing,
      vpcInterfaceName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the flow that is associated with the VPC interface you
-- removed.
removeFlowVpcInterfaceResponse_flowArn :: Lens.Lens' RemoveFlowVpcInterfaceResponse (Prelude.Maybe Prelude.Text)
removeFlowVpcInterfaceResponse_flowArn = Lens.lens (\RemoveFlowVpcInterfaceResponse' {flowArn} -> flowArn) (\s@RemoveFlowVpcInterfaceResponse' {} a -> s {flowArn = a} :: RemoveFlowVpcInterfaceResponse)

-- | IDs of network interfaces associated with the removed VPC interface that
-- Media Connect was unable to remove.
removeFlowVpcInterfaceResponse_nonDeletedNetworkInterfaceIds :: Lens.Lens' RemoveFlowVpcInterfaceResponse (Prelude.Maybe [Prelude.Text])
removeFlowVpcInterfaceResponse_nonDeletedNetworkInterfaceIds = Lens.lens (\RemoveFlowVpcInterfaceResponse' {nonDeletedNetworkInterfaceIds} -> nonDeletedNetworkInterfaceIds) (\s@RemoveFlowVpcInterfaceResponse' {} a -> s {nonDeletedNetworkInterfaceIds = a} :: RemoveFlowVpcInterfaceResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the VPC interface that was removed.
removeFlowVpcInterfaceResponse_vpcInterfaceName :: Lens.Lens' RemoveFlowVpcInterfaceResponse (Prelude.Maybe Prelude.Text)
removeFlowVpcInterfaceResponse_vpcInterfaceName = Lens.lens (\RemoveFlowVpcInterfaceResponse' {vpcInterfaceName} -> vpcInterfaceName) (\s@RemoveFlowVpcInterfaceResponse' {} a -> s {vpcInterfaceName = a} :: RemoveFlowVpcInterfaceResponse)

-- | The response's http status code.
removeFlowVpcInterfaceResponse_httpStatus :: Lens.Lens' RemoveFlowVpcInterfaceResponse Prelude.Int
removeFlowVpcInterfaceResponse_httpStatus = Lens.lens (\RemoveFlowVpcInterfaceResponse' {httpStatus} -> httpStatus) (\s@RemoveFlowVpcInterfaceResponse' {} a -> s {httpStatus = a} :: RemoveFlowVpcInterfaceResponse)

instance
  Prelude.NFData
    RemoveFlowVpcInterfaceResponse
  where
  rnf RemoveFlowVpcInterfaceResponse' {..} =
    Prelude.rnf flowArn
      `Prelude.seq` Prelude.rnf nonDeletedNetworkInterfaceIds
      `Prelude.seq` Prelude.rnf vpcInterfaceName
      `Prelude.seq` Prelude.rnf httpStatus
