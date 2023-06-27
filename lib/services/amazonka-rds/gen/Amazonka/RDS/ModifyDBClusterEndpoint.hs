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
-- Module      : Amazonka.RDS.ModifyDBClusterEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the properties of an endpoint in an Amazon Aurora DB cluster.
--
-- This action only applies to Aurora DB clusters.
module Amazonka.RDS.ModifyDBClusterEndpoint
  ( -- * Creating a Request
    ModifyDBClusterEndpoint (..),
    newModifyDBClusterEndpoint,

    -- * Request Lenses
    modifyDBClusterEndpoint_endpointType,
    modifyDBClusterEndpoint_excludedMembers,
    modifyDBClusterEndpoint_staticMembers,
    modifyDBClusterEndpoint_dbClusterEndpointIdentifier,

    -- * Destructuring the Response
    DBClusterEndpoint (..),
    newDBClusterEndpoint,

    -- * Response Lenses
    dbClusterEndpoint_customEndpointType,
    dbClusterEndpoint_dbClusterEndpointArn,
    dbClusterEndpoint_dbClusterEndpointIdentifier,
    dbClusterEndpoint_dbClusterEndpointResourceIdentifier,
    dbClusterEndpoint_dbClusterIdentifier,
    dbClusterEndpoint_endpoint,
    dbClusterEndpoint_endpointType,
    dbClusterEndpoint_excludedMembers,
    dbClusterEndpoint_staticMembers,
    dbClusterEndpoint_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyDBClusterEndpoint' smart constructor.
data ModifyDBClusterEndpoint = ModifyDBClusterEndpoint'
  { -- | The type of the endpoint. One of: @READER@, @WRITER@, @ANY@.
    endpointType :: Prelude.Maybe Prelude.Text,
    -- | List of DB instance identifiers that aren\'t part of the custom endpoint
    -- group. All other eligible instances are reachable through the custom
    -- endpoint. Only relevant if the list of static members is empty.
    excludedMembers :: Prelude.Maybe [Prelude.Text],
    -- | List of DB instance identifiers that are part of the custom endpoint
    -- group.
    staticMembers :: Prelude.Maybe [Prelude.Text],
    -- | The identifier of the endpoint to modify. This parameter is stored as a
    -- lowercase string.
    dbClusterEndpointIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyDBClusterEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointType', 'modifyDBClusterEndpoint_endpointType' - The type of the endpoint. One of: @READER@, @WRITER@, @ANY@.
--
-- 'excludedMembers', 'modifyDBClusterEndpoint_excludedMembers' - List of DB instance identifiers that aren\'t part of the custom endpoint
-- group. All other eligible instances are reachable through the custom
-- endpoint. Only relevant if the list of static members is empty.
--
-- 'staticMembers', 'modifyDBClusterEndpoint_staticMembers' - List of DB instance identifiers that are part of the custom endpoint
-- group.
--
-- 'dbClusterEndpointIdentifier', 'modifyDBClusterEndpoint_dbClusterEndpointIdentifier' - The identifier of the endpoint to modify. This parameter is stored as a
-- lowercase string.
newModifyDBClusterEndpoint ::
  -- | 'dbClusterEndpointIdentifier'
  Prelude.Text ->
  ModifyDBClusterEndpoint
newModifyDBClusterEndpoint
  pDBClusterEndpointIdentifier_ =
    ModifyDBClusterEndpoint'
      { endpointType =
          Prelude.Nothing,
        excludedMembers = Prelude.Nothing,
        staticMembers = Prelude.Nothing,
        dbClusterEndpointIdentifier =
          pDBClusterEndpointIdentifier_
      }

-- | The type of the endpoint. One of: @READER@, @WRITER@, @ANY@.
modifyDBClusterEndpoint_endpointType :: Lens.Lens' ModifyDBClusterEndpoint (Prelude.Maybe Prelude.Text)
modifyDBClusterEndpoint_endpointType = Lens.lens (\ModifyDBClusterEndpoint' {endpointType} -> endpointType) (\s@ModifyDBClusterEndpoint' {} a -> s {endpointType = a} :: ModifyDBClusterEndpoint)

-- | List of DB instance identifiers that aren\'t part of the custom endpoint
-- group. All other eligible instances are reachable through the custom
-- endpoint. Only relevant if the list of static members is empty.
modifyDBClusterEndpoint_excludedMembers :: Lens.Lens' ModifyDBClusterEndpoint (Prelude.Maybe [Prelude.Text])
modifyDBClusterEndpoint_excludedMembers = Lens.lens (\ModifyDBClusterEndpoint' {excludedMembers} -> excludedMembers) (\s@ModifyDBClusterEndpoint' {} a -> s {excludedMembers = a} :: ModifyDBClusterEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | List of DB instance identifiers that are part of the custom endpoint
-- group.
modifyDBClusterEndpoint_staticMembers :: Lens.Lens' ModifyDBClusterEndpoint (Prelude.Maybe [Prelude.Text])
modifyDBClusterEndpoint_staticMembers = Lens.lens (\ModifyDBClusterEndpoint' {staticMembers} -> staticMembers) (\s@ModifyDBClusterEndpoint' {} a -> s {staticMembers = a} :: ModifyDBClusterEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the endpoint to modify. This parameter is stored as a
-- lowercase string.
modifyDBClusterEndpoint_dbClusterEndpointIdentifier :: Lens.Lens' ModifyDBClusterEndpoint Prelude.Text
modifyDBClusterEndpoint_dbClusterEndpointIdentifier = Lens.lens (\ModifyDBClusterEndpoint' {dbClusterEndpointIdentifier} -> dbClusterEndpointIdentifier) (\s@ModifyDBClusterEndpoint' {} a -> s {dbClusterEndpointIdentifier = a} :: ModifyDBClusterEndpoint)

instance Core.AWSRequest ModifyDBClusterEndpoint where
  type
    AWSResponse ModifyDBClusterEndpoint =
      DBClusterEndpoint
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ModifyDBClusterEndpointResult"
      (\s h x -> Data.parseXML x)

instance Prelude.Hashable ModifyDBClusterEndpoint where
  hashWithSalt _salt ModifyDBClusterEndpoint' {..} =
    _salt
      `Prelude.hashWithSalt` endpointType
      `Prelude.hashWithSalt` excludedMembers
      `Prelude.hashWithSalt` staticMembers
      `Prelude.hashWithSalt` dbClusterEndpointIdentifier

instance Prelude.NFData ModifyDBClusterEndpoint where
  rnf ModifyDBClusterEndpoint' {..} =
    Prelude.rnf endpointType
      `Prelude.seq` Prelude.rnf excludedMembers
      `Prelude.seq` Prelude.rnf staticMembers
      `Prelude.seq` Prelude.rnf dbClusterEndpointIdentifier

instance Data.ToHeaders ModifyDBClusterEndpoint where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyDBClusterEndpoint where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyDBClusterEndpoint where
  toQuery ModifyDBClusterEndpoint' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ModifyDBClusterEndpoint" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "EndpointType" Data.=: endpointType,
        "ExcludedMembers"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> excludedMembers
            ),
        "StaticMembers"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> staticMembers
            ),
        "DBClusterEndpointIdentifier"
          Data.=: dbClusterEndpointIdentifier
      ]
