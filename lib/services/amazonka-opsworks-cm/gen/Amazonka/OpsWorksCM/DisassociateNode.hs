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
-- Module      : Amazonka.OpsWorksCM.DisassociateNode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a node from an AWS OpsWorks CM server, and removes the
-- node from the server\'s managed nodes. After a node is disassociated,
-- the node key pair is no longer valid for accessing the configuration
-- manager\'s API. For more information about how to associate a node, see
-- AssociateNode.
--
-- A node can can only be disassociated from a server that is in a
-- @HEALTHY@ state. Otherwise, an @InvalidStateException@ is thrown. A
-- @ResourceNotFoundException@ is thrown when the server does not exist. A
-- @ValidationException@ is raised when parameters of the request are not
-- valid.
module Amazonka.OpsWorksCM.DisassociateNode
  ( -- * Creating a Request
    DisassociateNode (..),
    newDisassociateNode,

    -- * Request Lenses
    disassociateNode_engineAttributes,
    disassociateNode_serverName,
    disassociateNode_nodeName,

    -- * Destructuring the Response
    DisassociateNodeResponse (..),
    newDisassociateNodeResponse,

    -- * Response Lenses
    disassociateNodeResponse_nodeAssociationStatusToken,
    disassociateNodeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorksCM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateNode' smart constructor.
data DisassociateNode = DisassociateNode'
  { -- | Engine attributes that are used for disassociating the node. No
    -- attributes are required for Puppet.
    --
    -- __Attributes required in a DisassociateNode request for Chef__
    --
    -- -   @CHEF_ORGANIZATION@: The Chef organization with which the node was
    --     associated. By default only one organization named @default@ can
    --     exist.
    engineAttributes :: Prelude.Maybe [EngineAttribute],
    -- | The name of the server from which to disassociate the node.
    serverName :: Prelude.Text,
    -- | The name of the client node.
    nodeName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateNode' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'engineAttributes', 'disassociateNode_engineAttributes' - Engine attributes that are used for disassociating the node. No
-- attributes are required for Puppet.
--
-- __Attributes required in a DisassociateNode request for Chef__
--
-- -   @CHEF_ORGANIZATION@: The Chef organization with which the node was
--     associated. By default only one organization named @default@ can
--     exist.
--
-- 'serverName', 'disassociateNode_serverName' - The name of the server from which to disassociate the node.
--
-- 'nodeName', 'disassociateNode_nodeName' - The name of the client node.
newDisassociateNode ::
  -- | 'serverName'
  Prelude.Text ->
  -- | 'nodeName'
  Prelude.Text ->
  DisassociateNode
newDisassociateNode pServerName_ pNodeName_ =
  DisassociateNode'
    { engineAttributes =
        Prelude.Nothing,
      serverName = pServerName_,
      nodeName = pNodeName_
    }

-- | Engine attributes that are used for disassociating the node. No
-- attributes are required for Puppet.
--
-- __Attributes required in a DisassociateNode request for Chef__
--
-- -   @CHEF_ORGANIZATION@: The Chef organization with which the node was
--     associated. By default only one organization named @default@ can
--     exist.
disassociateNode_engineAttributes :: Lens.Lens' DisassociateNode (Prelude.Maybe [EngineAttribute])
disassociateNode_engineAttributes = Lens.lens (\DisassociateNode' {engineAttributes} -> engineAttributes) (\s@DisassociateNode' {} a -> s {engineAttributes = a} :: DisassociateNode) Prelude.. Lens.mapping Lens.coerced

-- | The name of the server from which to disassociate the node.
disassociateNode_serverName :: Lens.Lens' DisassociateNode Prelude.Text
disassociateNode_serverName = Lens.lens (\DisassociateNode' {serverName} -> serverName) (\s@DisassociateNode' {} a -> s {serverName = a} :: DisassociateNode)

-- | The name of the client node.
disassociateNode_nodeName :: Lens.Lens' DisassociateNode Prelude.Text
disassociateNode_nodeName = Lens.lens (\DisassociateNode' {nodeName} -> nodeName) (\s@DisassociateNode' {} a -> s {nodeName = a} :: DisassociateNode)

instance Core.AWSRequest DisassociateNode where
  type
    AWSResponse DisassociateNode =
      DisassociateNodeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DisassociateNodeResponse'
            Prelude.<$> (x Data..?> "NodeAssociationStatusToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisassociateNode where
  hashWithSalt _salt DisassociateNode' {..} =
    _salt `Prelude.hashWithSalt` engineAttributes
      `Prelude.hashWithSalt` serverName
      `Prelude.hashWithSalt` nodeName

instance Prelude.NFData DisassociateNode where
  rnf DisassociateNode' {..} =
    Prelude.rnf engineAttributes
      `Prelude.seq` Prelude.rnf serverName
      `Prelude.seq` Prelude.rnf nodeName

instance Data.ToHeaders DisassociateNode where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpsWorksCM_V2016_11_01.DisassociateNode" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisassociateNode where
  toJSON DisassociateNode' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EngineAttributes" Data..=)
              Prelude.<$> engineAttributes,
            Prelude.Just ("ServerName" Data..= serverName),
            Prelude.Just ("NodeName" Data..= nodeName)
          ]
      )

instance Data.ToPath DisassociateNode where
  toPath = Prelude.const "/"

instance Data.ToQuery DisassociateNode where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateNodeResponse' smart constructor.
data DisassociateNodeResponse = DisassociateNodeResponse'
  { -- | Contains a token which can be passed to the
    -- @DescribeNodeAssociationStatus@ API call to get the status of the
    -- disassociation request.
    nodeAssociationStatusToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateNodeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nodeAssociationStatusToken', 'disassociateNodeResponse_nodeAssociationStatusToken' - Contains a token which can be passed to the
-- @DescribeNodeAssociationStatus@ API call to get the status of the
-- disassociation request.
--
-- 'httpStatus', 'disassociateNodeResponse_httpStatus' - The response's http status code.
newDisassociateNodeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateNodeResponse
newDisassociateNodeResponse pHttpStatus_ =
  DisassociateNodeResponse'
    { nodeAssociationStatusToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Contains a token which can be passed to the
-- @DescribeNodeAssociationStatus@ API call to get the status of the
-- disassociation request.
disassociateNodeResponse_nodeAssociationStatusToken :: Lens.Lens' DisassociateNodeResponse (Prelude.Maybe Prelude.Text)
disassociateNodeResponse_nodeAssociationStatusToken = Lens.lens (\DisassociateNodeResponse' {nodeAssociationStatusToken} -> nodeAssociationStatusToken) (\s@DisassociateNodeResponse' {} a -> s {nodeAssociationStatusToken = a} :: DisassociateNodeResponse)

-- | The response's http status code.
disassociateNodeResponse_httpStatus :: Lens.Lens' DisassociateNodeResponse Prelude.Int
disassociateNodeResponse_httpStatus = Lens.lens (\DisassociateNodeResponse' {httpStatus} -> httpStatus) (\s@DisassociateNodeResponse' {} a -> s {httpStatus = a} :: DisassociateNodeResponse)

instance Prelude.NFData DisassociateNodeResponse where
  rnf DisassociateNodeResponse' {..} =
    Prelude.rnf nodeAssociationStatusToken
      `Prelude.seq` Prelude.rnf httpStatus
