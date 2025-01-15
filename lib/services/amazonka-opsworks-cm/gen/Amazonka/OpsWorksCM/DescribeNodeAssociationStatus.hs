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
-- Module      : Amazonka.OpsWorksCM.DescribeNodeAssociationStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the current status of an existing association or disassociation
-- request.
--
-- A @ResourceNotFoundException@ is thrown when no recent association or
-- disassociation request with the specified token is found, or when the
-- server does not exist. A @ValidationException@ is raised when parameters
-- of the request are not valid.
module Amazonka.OpsWorksCM.DescribeNodeAssociationStatus
  ( -- * Creating a Request
    DescribeNodeAssociationStatus (..),
    newDescribeNodeAssociationStatus,

    -- * Request Lenses
    describeNodeAssociationStatus_nodeAssociationStatusToken,
    describeNodeAssociationStatus_serverName,

    -- * Destructuring the Response
    DescribeNodeAssociationStatusResponse (..),
    newDescribeNodeAssociationStatusResponse,

    -- * Response Lenses
    describeNodeAssociationStatusResponse_engineAttributes,
    describeNodeAssociationStatusResponse_httpStatus,
    describeNodeAssociationStatusResponse_nodeAssociationStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorksCM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeNodeAssociationStatus' smart constructor.
data DescribeNodeAssociationStatus = DescribeNodeAssociationStatus'
  { -- | The token returned in either the AssociateNodeResponse or the
    -- DisassociateNodeResponse.
    nodeAssociationStatusToken :: Prelude.Text,
    -- | The name of the server from which to disassociate the node.
    serverName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeNodeAssociationStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nodeAssociationStatusToken', 'describeNodeAssociationStatus_nodeAssociationStatusToken' - The token returned in either the AssociateNodeResponse or the
-- DisassociateNodeResponse.
--
-- 'serverName', 'describeNodeAssociationStatus_serverName' - The name of the server from which to disassociate the node.
newDescribeNodeAssociationStatus ::
  -- | 'nodeAssociationStatusToken'
  Prelude.Text ->
  -- | 'serverName'
  Prelude.Text ->
  DescribeNodeAssociationStatus
newDescribeNodeAssociationStatus
  pNodeAssociationStatusToken_
  pServerName_ =
    DescribeNodeAssociationStatus'
      { nodeAssociationStatusToken =
          pNodeAssociationStatusToken_,
        serverName = pServerName_
      }

-- | The token returned in either the AssociateNodeResponse or the
-- DisassociateNodeResponse.
describeNodeAssociationStatus_nodeAssociationStatusToken :: Lens.Lens' DescribeNodeAssociationStatus Prelude.Text
describeNodeAssociationStatus_nodeAssociationStatusToken = Lens.lens (\DescribeNodeAssociationStatus' {nodeAssociationStatusToken} -> nodeAssociationStatusToken) (\s@DescribeNodeAssociationStatus' {} a -> s {nodeAssociationStatusToken = a} :: DescribeNodeAssociationStatus)

-- | The name of the server from which to disassociate the node.
describeNodeAssociationStatus_serverName :: Lens.Lens' DescribeNodeAssociationStatus Prelude.Text
describeNodeAssociationStatus_serverName = Lens.lens (\DescribeNodeAssociationStatus' {serverName} -> serverName) (\s@DescribeNodeAssociationStatus' {} a -> s {serverName = a} :: DescribeNodeAssociationStatus)

instance
  Core.AWSRequest
    DescribeNodeAssociationStatus
  where
  type
    AWSResponse DescribeNodeAssociationStatus =
      DescribeNodeAssociationStatusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeNodeAssociationStatusResponse'
            Prelude.<$> ( x
                            Data..?> "EngineAttributes"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "NodeAssociationStatus")
      )

instance
  Prelude.Hashable
    DescribeNodeAssociationStatus
  where
  hashWithSalt _salt DescribeNodeAssociationStatus' {..} =
    _salt
      `Prelude.hashWithSalt` nodeAssociationStatusToken
      `Prelude.hashWithSalt` serverName

instance Prelude.NFData DescribeNodeAssociationStatus where
  rnf DescribeNodeAssociationStatus' {..} =
    Prelude.rnf nodeAssociationStatusToken `Prelude.seq`
      Prelude.rnf serverName

instance Data.ToHeaders DescribeNodeAssociationStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpsWorksCM_V2016_11_01.DescribeNodeAssociationStatus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeNodeAssociationStatus where
  toJSON DescribeNodeAssociationStatus' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "NodeAssociationStatusToken"
                  Data..= nodeAssociationStatusToken
              ),
            Prelude.Just ("ServerName" Data..= serverName)
          ]
      )

instance Data.ToPath DescribeNodeAssociationStatus where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeNodeAssociationStatus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeNodeAssociationStatusResponse' smart constructor.
data DescribeNodeAssociationStatusResponse = DescribeNodeAssociationStatusResponse'
  { -- | Attributes specific to the node association. In Puppet, the attibute
    -- PUPPET_NODE_CERT contains the signed certificate (the result of the
    -- CSR).
    engineAttributes :: Prelude.Maybe [EngineAttribute],
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The status of the association or disassociation request.
    --
    -- __Possible values:__
    --
    -- -   @SUCCESS@: The association or disassociation succeeded.
    --
    -- -   @FAILED@: The association or disassociation failed.
    --
    -- -   @IN_PROGRESS@: The association or disassociation is still in
    --     progress.
    nodeAssociationStatus :: NodeAssociationStatus
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeNodeAssociationStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'engineAttributes', 'describeNodeAssociationStatusResponse_engineAttributes' - Attributes specific to the node association. In Puppet, the attibute
-- PUPPET_NODE_CERT contains the signed certificate (the result of the
-- CSR).
--
-- 'httpStatus', 'describeNodeAssociationStatusResponse_httpStatus' - The response's http status code.
--
-- 'nodeAssociationStatus', 'describeNodeAssociationStatusResponse_nodeAssociationStatus' - The status of the association or disassociation request.
--
-- __Possible values:__
--
-- -   @SUCCESS@: The association or disassociation succeeded.
--
-- -   @FAILED@: The association or disassociation failed.
--
-- -   @IN_PROGRESS@: The association or disassociation is still in
--     progress.
newDescribeNodeAssociationStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'nodeAssociationStatus'
  NodeAssociationStatus ->
  DescribeNodeAssociationStatusResponse
newDescribeNodeAssociationStatusResponse
  pHttpStatus_
  pNodeAssociationStatus_ =
    DescribeNodeAssociationStatusResponse'
      { engineAttributes =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        nodeAssociationStatus =
          pNodeAssociationStatus_
      }

-- | Attributes specific to the node association. In Puppet, the attibute
-- PUPPET_NODE_CERT contains the signed certificate (the result of the
-- CSR).
describeNodeAssociationStatusResponse_engineAttributes :: Lens.Lens' DescribeNodeAssociationStatusResponse (Prelude.Maybe [EngineAttribute])
describeNodeAssociationStatusResponse_engineAttributes = Lens.lens (\DescribeNodeAssociationStatusResponse' {engineAttributes} -> engineAttributes) (\s@DescribeNodeAssociationStatusResponse' {} a -> s {engineAttributes = a} :: DescribeNodeAssociationStatusResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeNodeAssociationStatusResponse_httpStatus :: Lens.Lens' DescribeNodeAssociationStatusResponse Prelude.Int
describeNodeAssociationStatusResponse_httpStatus = Lens.lens (\DescribeNodeAssociationStatusResponse' {httpStatus} -> httpStatus) (\s@DescribeNodeAssociationStatusResponse' {} a -> s {httpStatus = a} :: DescribeNodeAssociationStatusResponse)

-- | The status of the association or disassociation request.
--
-- __Possible values:__
--
-- -   @SUCCESS@: The association or disassociation succeeded.
--
-- -   @FAILED@: The association or disassociation failed.
--
-- -   @IN_PROGRESS@: The association or disassociation is still in
--     progress.
describeNodeAssociationStatusResponse_nodeAssociationStatus :: Lens.Lens' DescribeNodeAssociationStatusResponse NodeAssociationStatus
describeNodeAssociationStatusResponse_nodeAssociationStatus = Lens.lens (\DescribeNodeAssociationStatusResponse' {nodeAssociationStatus} -> nodeAssociationStatus) (\s@DescribeNodeAssociationStatusResponse' {} a -> s {nodeAssociationStatus = a} :: DescribeNodeAssociationStatusResponse)

instance
  Prelude.NFData
    DescribeNodeAssociationStatusResponse
  where
  rnf DescribeNodeAssociationStatusResponse' {..} =
    Prelude.rnf engineAttributes `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf nodeAssociationStatus
