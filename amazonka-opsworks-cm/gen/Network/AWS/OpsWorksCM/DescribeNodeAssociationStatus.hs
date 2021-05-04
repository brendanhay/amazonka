{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.OpsWorksCM.DescribeNodeAssociationStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
module Network.AWS.OpsWorksCM.DescribeNodeAssociationStatus
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorksCM.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeNodeAssociationStatus' smart constructor.
data DescribeNodeAssociationStatus = DescribeNodeAssociationStatus'
  { -- | The token returned in either the AssociateNodeResponse or the
    -- DisassociateNodeResponse.
    nodeAssociationStatusToken :: Prelude.Text,
    -- | The name of the server from which to disassociate the node.
    serverName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.AWSRequest
    DescribeNodeAssociationStatus
  where
  type
    Rs DescribeNodeAssociationStatus =
      DescribeNodeAssociationStatusResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeNodeAssociationStatusResponse'
            Prelude.<$> ( x Prelude..?> "EngineAttributes"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "NodeAssociationStatus")
      )

instance
  Prelude.Hashable
    DescribeNodeAssociationStatus

instance Prelude.NFData DescribeNodeAssociationStatus

instance
  Prelude.ToHeaders
    DescribeNodeAssociationStatus
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "OpsWorksCM_V2016_11_01.DescribeNodeAssociationStatus" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeNodeAssociationStatus where
  toJSON DescribeNodeAssociationStatus' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "NodeAssociationStatusToken"
                  Prelude..= nodeAssociationStatusToken
              ),
            Prelude.Just ("ServerName" Prelude..= serverName)
          ]
      )

instance Prelude.ToPath DescribeNodeAssociationStatus where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DescribeNodeAssociationStatus
  where
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
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
describeNodeAssociationStatusResponse_engineAttributes = Lens.lens (\DescribeNodeAssociationStatusResponse' {engineAttributes} -> engineAttributes) (\s@DescribeNodeAssociationStatusResponse' {} a -> s {engineAttributes = a} :: DescribeNodeAssociationStatusResponse) Prelude.. Lens.mapping Prelude._Coerce

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
