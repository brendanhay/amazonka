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
-- Module      : Network.AWS.RDS.ModifyDBClusterEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the properties of an endpoint in an Amazon Aurora DB cluster.
--
-- This action only applies to Aurora DB clusters.
module Network.AWS.RDS.ModifyDBClusterEndpoint
  ( -- * Creating a Request
    ModifyDBClusterEndpoint (..),
    newModifyDBClusterEndpoint,

    -- * Request Lenses
    modifyDBClusterEndpoint_excludedMembers,
    modifyDBClusterEndpoint_endpointType,
    modifyDBClusterEndpoint_staticMembers,
    modifyDBClusterEndpoint_dbClusterEndpointIdentifier,

    -- * Destructuring the Response
    DBClusterEndpoint (..),
    newDBClusterEndpoint,

    -- * Response Lenses
    dbClusterEndpoint_dbClusterEndpointIdentifier,
    dbClusterEndpoint_status,
    dbClusterEndpoint_excludedMembers,
    dbClusterEndpoint_endpointType,
    dbClusterEndpoint_customEndpointType,
    dbClusterEndpoint_dbClusterEndpointArn,
    dbClusterEndpoint_dbClusterIdentifier,
    dbClusterEndpoint_dbClusterEndpointResourceIdentifier,
    dbClusterEndpoint_endpoint,
    dbClusterEndpoint_staticMembers,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyDBClusterEndpoint' smart constructor.
data ModifyDBClusterEndpoint = ModifyDBClusterEndpoint'
  { -- | List of DB instance identifiers that aren\'t part of the custom endpoint
    -- group. All other eligible instances are reachable through the custom
    -- endpoint. Only relevant if the list of static members is empty.
    excludedMembers :: Core.Maybe [Core.Text],
    -- | The type of the endpoint. One of: @READER@, @WRITER@, @ANY@.
    endpointType :: Core.Maybe Core.Text,
    -- | List of DB instance identifiers that are part of the custom endpoint
    -- group.
    staticMembers :: Core.Maybe [Core.Text],
    -- | The identifier of the endpoint to modify. This parameter is stored as a
    -- lowercase string.
    dbClusterEndpointIdentifier :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyDBClusterEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'excludedMembers', 'modifyDBClusterEndpoint_excludedMembers' - List of DB instance identifiers that aren\'t part of the custom endpoint
-- group. All other eligible instances are reachable through the custom
-- endpoint. Only relevant if the list of static members is empty.
--
-- 'endpointType', 'modifyDBClusterEndpoint_endpointType' - The type of the endpoint. One of: @READER@, @WRITER@, @ANY@.
--
-- 'staticMembers', 'modifyDBClusterEndpoint_staticMembers' - List of DB instance identifiers that are part of the custom endpoint
-- group.
--
-- 'dbClusterEndpointIdentifier', 'modifyDBClusterEndpoint_dbClusterEndpointIdentifier' - The identifier of the endpoint to modify. This parameter is stored as a
-- lowercase string.
newModifyDBClusterEndpoint ::
  -- | 'dbClusterEndpointIdentifier'
  Core.Text ->
  ModifyDBClusterEndpoint
newModifyDBClusterEndpoint
  pDBClusterEndpointIdentifier_ =
    ModifyDBClusterEndpoint'
      { excludedMembers =
          Core.Nothing,
        endpointType = Core.Nothing,
        staticMembers = Core.Nothing,
        dbClusterEndpointIdentifier =
          pDBClusterEndpointIdentifier_
      }

-- | List of DB instance identifiers that aren\'t part of the custom endpoint
-- group. All other eligible instances are reachable through the custom
-- endpoint. Only relevant if the list of static members is empty.
modifyDBClusterEndpoint_excludedMembers :: Lens.Lens' ModifyDBClusterEndpoint (Core.Maybe [Core.Text])
modifyDBClusterEndpoint_excludedMembers = Lens.lens (\ModifyDBClusterEndpoint' {excludedMembers} -> excludedMembers) (\s@ModifyDBClusterEndpoint' {} a -> s {excludedMembers = a} :: ModifyDBClusterEndpoint) Core.. Lens.mapping Lens._Coerce

-- | The type of the endpoint. One of: @READER@, @WRITER@, @ANY@.
modifyDBClusterEndpoint_endpointType :: Lens.Lens' ModifyDBClusterEndpoint (Core.Maybe Core.Text)
modifyDBClusterEndpoint_endpointType = Lens.lens (\ModifyDBClusterEndpoint' {endpointType} -> endpointType) (\s@ModifyDBClusterEndpoint' {} a -> s {endpointType = a} :: ModifyDBClusterEndpoint)

-- | List of DB instance identifiers that are part of the custom endpoint
-- group.
modifyDBClusterEndpoint_staticMembers :: Lens.Lens' ModifyDBClusterEndpoint (Core.Maybe [Core.Text])
modifyDBClusterEndpoint_staticMembers = Lens.lens (\ModifyDBClusterEndpoint' {staticMembers} -> staticMembers) (\s@ModifyDBClusterEndpoint' {} a -> s {staticMembers = a} :: ModifyDBClusterEndpoint) Core.. Lens.mapping Lens._Coerce

-- | The identifier of the endpoint to modify. This parameter is stored as a
-- lowercase string.
modifyDBClusterEndpoint_dbClusterEndpointIdentifier :: Lens.Lens' ModifyDBClusterEndpoint Core.Text
modifyDBClusterEndpoint_dbClusterEndpointIdentifier = Lens.lens (\ModifyDBClusterEndpoint' {dbClusterEndpointIdentifier} -> dbClusterEndpointIdentifier) (\s@ModifyDBClusterEndpoint' {} a -> s {dbClusterEndpointIdentifier = a} :: ModifyDBClusterEndpoint)

instance Core.AWSRequest ModifyDBClusterEndpoint where
  type
    AWSResponse ModifyDBClusterEndpoint =
      DBClusterEndpoint
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ModifyDBClusterEndpointResult"
      (\s h x -> Core.parseXML x)

instance Core.Hashable ModifyDBClusterEndpoint

instance Core.NFData ModifyDBClusterEndpoint

instance Core.ToHeaders ModifyDBClusterEndpoint where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ModifyDBClusterEndpoint where
  toPath = Core.const "/"

instance Core.ToQuery ModifyDBClusterEndpoint where
  toQuery ModifyDBClusterEndpoint' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ModifyDBClusterEndpoint" :: Core.ByteString),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "ExcludedMembers"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> excludedMembers),
        "EndpointType" Core.=: endpointType,
        "StaticMembers"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> staticMembers),
        "DBClusterEndpointIdentifier"
          Core.=: dbClusterEndpointIdentifier
      ]
