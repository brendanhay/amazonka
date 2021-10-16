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
-- Module      : Network.AWS.RDS.CreateDBClusterEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new custom endpoint and associates it with an Amazon Aurora DB
-- cluster.
--
-- This action only applies to Aurora DB clusters.
module Network.AWS.RDS.CreateDBClusterEndpoint
  ( -- * Creating a Request
    CreateDBClusterEndpoint (..),
    newCreateDBClusterEndpoint,

    -- * Request Lenses
    createDBClusterEndpoint_excludedMembers,
    createDBClusterEndpoint_tags,
    createDBClusterEndpoint_staticMembers,
    createDBClusterEndpoint_dbClusterIdentifier,
    createDBClusterEndpoint_dbClusterEndpointIdentifier,
    createDBClusterEndpoint_endpointType,

    -- * Destructuring the Response
    DBClusterEndpoint (..),
    newDBClusterEndpoint,

    -- * Response Lenses
    dbClusterEndpoint_status,
    dbClusterEndpoint_dbClusterEndpointIdentifier,
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
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateDBClusterEndpoint' smart constructor.
data CreateDBClusterEndpoint = CreateDBClusterEndpoint'
  { -- | List of DB instance identifiers that aren\'t part of the custom endpoint
    -- group. All other eligible instances are reachable through the custom
    -- endpoint. Only relevant if the list of static members is empty.
    excludedMembers :: Prelude.Maybe [Prelude.Text],
    -- | The tags to be assigned to the Amazon RDS resource.
    tags :: Prelude.Maybe [Tag],
    -- | List of DB instance identifiers that are part of the custom endpoint
    -- group.
    staticMembers :: Prelude.Maybe [Prelude.Text],
    -- | The DB cluster identifier of the DB cluster associated with the
    -- endpoint. This parameter is stored as a lowercase string.
    dbClusterIdentifier :: Prelude.Text,
    -- | The identifier to use for the new endpoint. This parameter is stored as
    -- a lowercase string.
    dbClusterEndpointIdentifier :: Prelude.Text,
    -- | The type of the endpoint. One of: @READER@, @WRITER@, @ANY@.
    endpointType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDBClusterEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'excludedMembers', 'createDBClusterEndpoint_excludedMembers' - List of DB instance identifiers that aren\'t part of the custom endpoint
-- group. All other eligible instances are reachable through the custom
-- endpoint. Only relevant if the list of static members is empty.
--
-- 'tags', 'createDBClusterEndpoint_tags' - The tags to be assigned to the Amazon RDS resource.
--
-- 'staticMembers', 'createDBClusterEndpoint_staticMembers' - List of DB instance identifiers that are part of the custom endpoint
-- group.
--
-- 'dbClusterIdentifier', 'createDBClusterEndpoint_dbClusterIdentifier' - The DB cluster identifier of the DB cluster associated with the
-- endpoint. This parameter is stored as a lowercase string.
--
-- 'dbClusterEndpointIdentifier', 'createDBClusterEndpoint_dbClusterEndpointIdentifier' - The identifier to use for the new endpoint. This parameter is stored as
-- a lowercase string.
--
-- 'endpointType', 'createDBClusterEndpoint_endpointType' - The type of the endpoint. One of: @READER@, @WRITER@, @ANY@.
newCreateDBClusterEndpoint ::
  -- | 'dbClusterIdentifier'
  Prelude.Text ->
  -- | 'dbClusterEndpointIdentifier'
  Prelude.Text ->
  -- | 'endpointType'
  Prelude.Text ->
  CreateDBClusterEndpoint
newCreateDBClusterEndpoint
  pDBClusterIdentifier_
  pDBClusterEndpointIdentifier_
  pEndpointType_ =
    CreateDBClusterEndpoint'
      { excludedMembers =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        staticMembers = Prelude.Nothing,
        dbClusterIdentifier = pDBClusterIdentifier_,
        dbClusterEndpointIdentifier =
          pDBClusterEndpointIdentifier_,
        endpointType = pEndpointType_
      }

-- | List of DB instance identifiers that aren\'t part of the custom endpoint
-- group. All other eligible instances are reachable through the custom
-- endpoint. Only relevant if the list of static members is empty.
createDBClusterEndpoint_excludedMembers :: Lens.Lens' CreateDBClusterEndpoint (Prelude.Maybe [Prelude.Text])
createDBClusterEndpoint_excludedMembers = Lens.lens (\CreateDBClusterEndpoint' {excludedMembers} -> excludedMembers) (\s@CreateDBClusterEndpoint' {} a -> s {excludedMembers = a} :: CreateDBClusterEndpoint) Prelude.. Lens.mapping Lens._Coerce

-- | The tags to be assigned to the Amazon RDS resource.
createDBClusterEndpoint_tags :: Lens.Lens' CreateDBClusterEndpoint (Prelude.Maybe [Tag])
createDBClusterEndpoint_tags = Lens.lens (\CreateDBClusterEndpoint' {tags} -> tags) (\s@CreateDBClusterEndpoint' {} a -> s {tags = a} :: CreateDBClusterEndpoint) Prelude.. Lens.mapping Lens._Coerce

-- | List of DB instance identifiers that are part of the custom endpoint
-- group.
createDBClusterEndpoint_staticMembers :: Lens.Lens' CreateDBClusterEndpoint (Prelude.Maybe [Prelude.Text])
createDBClusterEndpoint_staticMembers = Lens.lens (\CreateDBClusterEndpoint' {staticMembers} -> staticMembers) (\s@CreateDBClusterEndpoint' {} a -> s {staticMembers = a} :: CreateDBClusterEndpoint) Prelude.. Lens.mapping Lens._Coerce

-- | The DB cluster identifier of the DB cluster associated with the
-- endpoint. This parameter is stored as a lowercase string.
createDBClusterEndpoint_dbClusterIdentifier :: Lens.Lens' CreateDBClusterEndpoint Prelude.Text
createDBClusterEndpoint_dbClusterIdentifier = Lens.lens (\CreateDBClusterEndpoint' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@CreateDBClusterEndpoint' {} a -> s {dbClusterIdentifier = a} :: CreateDBClusterEndpoint)

-- | The identifier to use for the new endpoint. This parameter is stored as
-- a lowercase string.
createDBClusterEndpoint_dbClusterEndpointIdentifier :: Lens.Lens' CreateDBClusterEndpoint Prelude.Text
createDBClusterEndpoint_dbClusterEndpointIdentifier = Lens.lens (\CreateDBClusterEndpoint' {dbClusterEndpointIdentifier} -> dbClusterEndpointIdentifier) (\s@CreateDBClusterEndpoint' {} a -> s {dbClusterEndpointIdentifier = a} :: CreateDBClusterEndpoint)

-- | The type of the endpoint. One of: @READER@, @WRITER@, @ANY@.
createDBClusterEndpoint_endpointType :: Lens.Lens' CreateDBClusterEndpoint Prelude.Text
createDBClusterEndpoint_endpointType = Lens.lens (\CreateDBClusterEndpoint' {endpointType} -> endpointType) (\s@CreateDBClusterEndpoint' {} a -> s {endpointType = a} :: CreateDBClusterEndpoint)

instance Core.AWSRequest CreateDBClusterEndpoint where
  type
    AWSResponse CreateDBClusterEndpoint =
      DBClusterEndpoint
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CreateDBClusterEndpointResult"
      (\s h x -> Core.parseXML x)

instance Prelude.Hashable CreateDBClusterEndpoint

instance Prelude.NFData CreateDBClusterEndpoint

instance Core.ToHeaders CreateDBClusterEndpoint where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateDBClusterEndpoint where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateDBClusterEndpoint where
  toQuery CreateDBClusterEndpoint' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("CreateDBClusterEndpoint" :: Prelude.ByteString),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "ExcludedMembers"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> excludedMembers
            ),
        "Tags"
          Core.=: Core.toQuery
            (Core.toQueryList "Tag" Prelude.<$> tags),
        "StaticMembers"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> staticMembers
            ),
        "DBClusterIdentifier" Core.=: dbClusterIdentifier,
        "DBClusterEndpointIdentifier"
          Core.=: dbClusterEndpointIdentifier,
        "EndpointType" Core.=: endpointType
      ]
