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
-- Module      : Amazonka.Neptune.CreateDBClusterEndpoint
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new custom endpoint and associates it with an Amazon Neptune
-- DB cluster.
module Amazonka.Neptune.CreateDBClusterEndpoint
  ( -- * Creating a Request
    CreateDBClusterEndpoint (..),
    newCreateDBClusterEndpoint,

    -- * Request Lenses
    createDBClusterEndpoint_tags,
    createDBClusterEndpoint_staticMembers,
    createDBClusterEndpoint_excludedMembers,
    createDBClusterEndpoint_dbClusterIdentifier,
    createDBClusterEndpoint_dbClusterEndpointIdentifier,
    createDBClusterEndpoint_endpointType,

    -- * Destructuring the Response
    CreateDBClusterEndpointResponse (..),
    newCreateDBClusterEndpointResponse,

    -- * Response Lenses
    createDBClusterEndpointResponse_dbClusterEndpointResourceIdentifier,
    createDBClusterEndpointResponse_staticMembers,
    createDBClusterEndpointResponse_dbClusterIdentifier,
    createDBClusterEndpointResponse_excludedMembers,
    createDBClusterEndpointResponse_customEndpointType,
    createDBClusterEndpointResponse_status,
    createDBClusterEndpointResponse_endpointType,
    createDBClusterEndpointResponse_dbClusterEndpointIdentifier,
    createDBClusterEndpointResponse_dbClusterEndpointArn,
    createDBClusterEndpointResponse_endpoint,
    createDBClusterEndpointResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Neptune.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDBClusterEndpoint' smart constructor.
data CreateDBClusterEndpoint = CreateDBClusterEndpoint'
  { -- | The tags to be assigned to the Amazon Neptune resource.
    tags :: Prelude.Maybe [Tag],
    -- | List of DB instance identifiers that are part of the custom endpoint
    -- group.
    staticMembers :: Prelude.Maybe [Prelude.Text],
    -- | List of DB instance identifiers that aren\'t part of the custom endpoint
    -- group. All other eligible instances are reachable through the custom
    -- endpoint. Only relevant if the list of static members is empty.
    excludedMembers :: Prelude.Maybe [Prelude.Text],
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
-- 'tags', 'createDBClusterEndpoint_tags' - The tags to be assigned to the Amazon Neptune resource.
--
-- 'staticMembers', 'createDBClusterEndpoint_staticMembers' - List of DB instance identifiers that are part of the custom endpoint
-- group.
--
-- 'excludedMembers', 'createDBClusterEndpoint_excludedMembers' - List of DB instance identifiers that aren\'t part of the custom endpoint
-- group. All other eligible instances are reachable through the custom
-- endpoint. Only relevant if the list of static members is empty.
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
      { tags = Prelude.Nothing,
        staticMembers = Prelude.Nothing,
        excludedMembers = Prelude.Nothing,
        dbClusterIdentifier = pDBClusterIdentifier_,
        dbClusterEndpointIdentifier =
          pDBClusterEndpointIdentifier_,
        endpointType = pEndpointType_
      }

-- | The tags to be assigned to the Amazon Neptune resource.
createDBClusterEndpoint_tags :: Lens.Lens' CreateDBClusterEndpoint (Prelude.Maybe [Tag])
createDBClusterEndpoint_tags = Lens.lens (\CreateDBClusterEndpoint' {tags} -> tags) (\s@CreateDBClusterEndpoint' {} a -> s {tags = a} :: CreateDBClusterEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | List of DB instance identifiers that are part of the custom endpoint
-- group.
createDBClusterEndpoint_staticMembers :: Lens.Lens' CreateDBClusterEndpoint (Prelude.Maybe [Prelude.Text])
createDBClusterEndpoint_staticMembers = Lens.lens (\CreateDBClusterEndpoint' {staticMembers} -> staticMembers) (\s@CreateDBClusterEndpoint' {} a -> s {staticMembers = a} :: CreateDBClusterEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | List of DB instance identifiers that aren\'t part of the custom endpoint
-- group. All other eligible instances are reachable through the custom
-- endpoint. Only relevant if the list of static members is empty.
createDBClusterEndpoint_excludedMembers :: Lens.Lens' CreateDBClusterEndpoint (Prelude.Maybe [Prelude.Text])
createDBClusterEndpoint_excludedMembers = Lens.lens (\CreateDBClusterEndpoint' {excludedMembers} -> excludedMembers) (\s@CreateDBClusterEndpoint' {} a -> s {excludedMembers = a} :: CreateDBClusterEndpoint) Prelude.. Lens.mapping Lens.coerced

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
      CreateDBClusterEndpointResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CreateDBClusterEndpointResult"
      ( \s h x ->
          CreateDBClusterEndpointResponse'
            Prelude.<$> (x Data..@? "DBClusterEndpointResourceIdentifier")
            Prelude.<*> ( x Data..@? "StaticMembers" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (x Data..@? "DBClusterIdentifier")
            Prelude.<*> ( x Data..@? "ExcludedMembers" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (x Data..@? "CustomEndpointType")
            Prelude.<*> (x Data..@? "Status")
            Prelude.<*> (x Data..@? "EndpointType")
            Prelude.<*> (x Data..@? "DBClusterEndpointIdentifier")
            Prelude.<*> (x Data..@? "DBClusterEndpointArn")
            Prelude.<*> (x Data..@? "Endpoint")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDBClusterEndpoint where
  hashWithSalt _salt CreateDBClusterEndpoint' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` staticMembers
      `Prelude.hashWithSalt` excludedMembers
      `Prelude.hashWithSalt` dbClusterIdentifier
      `Prelude.hashWithSalt` dbClusterEndpointIdentifier
      `Prelude.hashWithSalt` endpointType

instance Prelude.NFData CreateDBClusterEndpoint where
  rnf CreateDBClusterEndpoint' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf staticMembers
      `Prelude.seq` Prelude.rnf excludedMembers
      `Prelude.seq` Prelude.rnf dbClusterIdentifier
      `Prelude.seq` Prelude.rnf dbClusterEndpointIdentifier
      `Prelude.seq` Prelude.rnf endpointType

instance Data.ToHeaders CreateDBClusterEndpoint where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateDBClusterEndpoint where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateDBClusterEndpoint where
  toQuery CreateDBClusterEndpoint' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateDBClusterEndpoint" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "Tags"
          Data.=: Data.toQuery
            (Data.toQueryList "Tag" Prelude.<$> tags),
        "StaticMembers"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> staticMembers
            ),
        "ExcludedMembers"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> excludedMembers
            ),
        "DBClusterIdentifier" Data.=: dbClusterIdentifier,
        "DBClusterEndpointIdentifier"
          Data.=: dbClusterEndpointIdentifier,
        "EndpointType" Data.=: endpointType
      ]

-- | This data type represents the information you need to connect to an
-- Amazon Neptune DB cluster. This data type is used as a response element
-- in the following actions:
--
-- -   @CreateDBClusterEndpoint@
--
-- -   @DescribeDBClusterEndpoints@
--
-- -   @ModifyDBClusterEndpoint@
--
-- -   @DeleteDBClusterEndpoint@
--
-- For the data structure that represents Amazon Neptune DB instance
-- endpoints, see @Endpoint@.
--
-- /See:/ 'newCreateDBClusterEndpointResponse' smart constructor.
data CreateDBClusterEndpointResponse = CreateDBClusterEndpointResponse'
  { -- | A unique system-generated identifier for an endpoint. It remains the
    -- same for the whole life of the endpoint.
    dbClusterEndpointResourceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | List of DB instance identifiers that are part of the custom endpoint
    -- group.
    staticMembers :: Prelude.Maybe [Prelude.Text],
    -- | The DB cluster identifier of the DB cluster associated with the
    -- endpoint. This parameter is stored as a lowercase string.
    dbClusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | List of DB instance identifiers that aren\'t part of the custom endpoint
    -- group. All other eligible instances are reachable through the custom
    -- endpoint. Only relevant if the list of static members is empty.
    excludedMembers :: Prelude.Maybe [Prelude.Text],
    -- | The type associated with a custom endpoint. One of: @READER@, @WRITER@,
    -- @ANY@.
    customEndpointType :: Prelude.Maybe Prelude.Text,
    -- | The current status of the endpoint. One of: @creating@, @available@,
    -- @deleting@, @inactive@, @modifying@. The @inactive@ state applies to an
    -- endpoint that cannot be used for a certain kind of cluster, such as a
    -- @writer@ endpoint for a read-only secondary cluster in a global
    -- database.
    status :: Prelude.Maybe Prelude.Text,
    -- | The type of the endpoint. One of: @READER@, @WRITER@, @CUSTOM@.
    endpointType :: Prelude.Maybe Prelude.Text,
    -- | The identifier associated with the endpoint. This parameter is stored as
    -- a lowercase string.
    dbClusterEndpointIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the endpoint.
    dbClusterEndpointArn :: Prelude.Maybe Prelude.Text,
    -- | The DNS address of the endpoint.
    endpoint :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDBClusterEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbClusterEndpointResourceIdentifier', 'createDBClusterEndpointResponse_dbClusterEndpointResourceIdentifier' - A unique system-generated identifier for an endpoint. It remains the
-- same for the whole life of the endpoint.
--
-- 'staticMembers', 'createDBClusterEndpointResponse_staticMembers' - List of DB instance identifiers that are part of the custom endpoint
-- group.
--
-- 'dbClusterIdentifier', 'createDBClusterEndpointResponse_dbClusterIdentifier' - The DB cluster identifier of the DB cluster associated with the
-- endpoint. This parameter is stored as a lowercase string.
--
-- 'excludedMembers', 'createDBClusterEndpointResponse_excludedMembers' - List of DB instance identifiers that aren\'t part of the custom endpoint
-- group. All other eligible instances are reachable through the custom
-- endpoint. Only relevant if the list of static members is empty.
--
-- 'customEndpointType', 'createDBClusterEndpointResponse_customEndpointType' - The type associated with a custom endpoint. One of: @READER@, @WRITER@,
-- @ANY@.
--
-- 'status', 'createDBClusterEndpointResponse_status' - The current status of the endpoint. One of: @creating@, @available@,
-- @deleting@, @inactive@, @modifying@. The @inactive@ state applies to an
-- endpoint that cannot be used for a certain kind of cluster, such as a
-- @writer@ endpoint for a read-only secondary cluster in a global
-- database.
--
-- 'endpointType', 'createDBClusterEndpointResponse_endpointType' - The type of the endpoint. One of: @READER@, @WRITER@, @CUSTOM@.
--
-- 'dbClusterEndpointIdentifier', 'createDBClusterEndpointResponse_dbClusterEndpointIdentifier' - The identifier associated with the endpoint. This parameter is stored as
-- a lowercase string.
--
-- 'dbClusterEndpointArn', 'createDBClusterEndpointResponse_dbClusterEndpointArn' - The Amazon Resource Name (ARN) for the endpoint.
--
-- 'endpoint', 'createDBClusterEndpointResponse_endpoint' - The DNS address of the endpoint.
--
-- 'httpStatus', 'createDBClusterEndpointResponse_httpStatus' - The response's http status code.
newCreateDBClusterEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDBClusterEndpointResponse
newCreateDBClusterEndpointResponse pHttpStatus_ =
  CreateDBClusterEndpointResponse'
    { dbClusterEndpointResourceIdentifier =
        Prelude.Nothing,
      staticMembers = Prelude.Nothing,
      dbClusterIdentifier = Prelude.Nothing,
      excludedMembers = Prelude.Nothing,
      customEndpointType = Prelude.Nothing,
      status = Prelude.Nothing,
      endpointType = Prelude.Nothing,
      dbClusterEndpointIdentifier =
        Prelude.Nothing,
      dbClusterEndpointArn = Prelude.Nothing,
      endpoint = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A unique system-generated identifier for an endpoint. It remains the
-- same for the whole life of the endpoint.
createDBClusterEndpointResponse_dbClusterEndpointResourceIdentifier :: Lens.Lens' CreateDBClusterEndpointResponse (Prelude.Maybe Prelude.Text)
createDBClusterEndpointResponse_dbClusterEndpointResourceIdentifier = Lens.lens (\CreateDBClusterEndpointResponse' {dbClusterEndpointResourceIdentifier} -> dbClusterEndpointResourceIdentifier) (\s@CreateDBClusterEndpointResponse' {} a -> s {dbClusterEndpointResourceIdentifier = a} :: CreateDBClusterEndpointResponse)

-- | List of DB instance identifiers that are part of the custom endpoint
-- group.
createDBClusterEndpointResponse_staticMembers :: Lens.Lens' CreateDBClusterEndpointResponse (Prelude.Maybe [Prelude.Text])
createDBClusterEndpointResponse_staticMembers = Lens.lens (\CreateDBClusterEndpointResponse' {staticMembers} -> staticMembers) (\s@CreateDBClusterEndpointResponse' {} a -> s {staticMembers = a} :: CreateDBClusterEndpointResponse) Prelude.. Lens.mapping Lens.coerced

-- | The DB cluster identifier of the DB cluster associated with the
-- endpoint. This parameter is stored as a lowercase string.
createDBClusterEndpointResponse_dbClusterIdentifier :: Lens.Lens' CreateDBClusterEndpointResponse (Prelude.Maybe Prelude.Text)
createDBClusterEndpointResponse_dbClusterIdentifier = Lens.lens (\CreateDBClusterEndpointResponse' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@CreateDBClusterEndpointResponse' {} a -> s {dbClusterIdentifier = a} :: CreateDBClusterEndpointResponse)

-- | List of DB instance identifiers that aren\'t part of the custom endpoint
-- group. All other eligible instances are reachable through the custom
-- endpoint. Only relevant if the list of static members is empty.
createDBClusterEndpointResponse_excludedMembers :: Lens.Lens' CreateDBClusterEndpointResponse (Prelude.Maybe [Prelude.Text])
createDBClusterEndpointResponse_excludedMembers = Lens.lens (\CreateDBClusterEndpointResponse' {excludedMembers} -> excludedMembers) (\s@CreateDBClusterEndpointResponse' {} a -> s {excludedMembers = a} :: CreateDBClusterEndpointResponse) Prelude.. Lens.mapping Lens.coerced

-- | The type associated with a custom endpoint. One of: @READER@, @WRITER@,
-- @ANY@.
createDBClusterEndpointResponse_customEndpointType :: Lens.Lens' CreateDBClusterEndpointResponse (Prelude.Maybe Prelude.Text)
createDBClusterEndpointResponse_customEndpointType = Lens.lens (\CreateDBClusterEndpointResponse' {customEndpointType} -> customEndpointType) (\s@CreateDBClusterEndpointResponse' {} a -> s {customEndpointType = a} :: CreateDBClusterEndpointResponse)

-- | The current status of the endpoint. One of: @creating@, @available@,
-- @deleting@, @inactive@, @modifying@. The @inactive@ state applies to an
-- endpoint that cannot be used for a certain kind of cluster, such as a
-- @writer@ endpoint for a read-only secondary cluster in a global
-- database.
createDBClusterEndpointResponse_status :: Lens.Lens' CreateDBClusterEndpointResponse (Prelude.Maybe Prelude.Text)
createDBClusterEndpointResponse_status = Lens.lens (\CreateDBClusterEndpointResponse' {status} -> status) (\s@CreateDBClusterEndpointResponse' {} a -> s {status = a} :: CreateDBClusterEndpointResponse)

-- | The type of the endpoint. One of: @READER@, @WRITER@, @CUSTOM@.
createDBClusterEndpointResponse_endpointType :: Lens.Lens' CreateDBClusterEndpointResponse (Prelude.Maybe Prelude.Text)
createDBClusterEndpointResponse_endpointType = Lens.lens (\CreateDBClusterEndpointResponse' {endpointType} -> endpointType) (\s@CreateDBClusterEndpointResponse' {} a -> s {endpointType = a} :: CreateDBClusterEndpointResponse)

-- | The identifier associated with the endpoint. This parameter is stored as
-- a lowercase string.
createDBClusterEndpointResponse_dbClusterEndpointIdentifier :: Lens.Lens' CreateDBClusterEndpointResponse (Prelude.Maybe Prelude.Text)
createDBClusterEndpointResponse_dbClusterEndpointIdentifier = Lens.lens (\CreateDBClusterEndpointResponse' {dbClusterEndpointIdentifier} -> dbClusterEndpointIdentifier) (\s@CreateDBClusterEndpointResponse' {} a -> s {dbClusterEndpointIdentifier = a} :: CreateDBClusterEndpointResponse)

-- | The Amazon Resource Name (ARN) for the endpoint.
createDBClusterEndpointResponse_dbClusterEndpointArn :: Lens.Lens' CreateDBClusterEndpointResponse (Prelude.Maybe Prelude.Text)
createDBClusterEndpointResponse_dbClusterEndpointArn = Lens.lens (\CreateDBClusterEndpointResponse' {dbClusterEndpointArn} -> dbClusterEndpointArn) (\s@CreateDBClusterEndpointResponse' {} a -> s {dbClusterEndpointArn = a} :: CreateDBClusterEndpointResponse)

-- | The DNS address of the endpoint.
createDBClusterEndpointResponse_endpoint :: Lens.Lens' CreateDBClusterEndpointResponse (Prelude.Maybe Prelude.Text)
createDBClusterEndpointResponse_endpoint = Lens.lens (\CreateDBClusterEndpointResponse' {endpoint} -> endpoint) (\s@CreateDBClusterEndpointResponse' {} a -> s {endpoint = a} :: CreateDBClusterEndpointResponse)

-- | The response's http status code.
createDBClusterEndpointResponse_httpStatus :: Lens.Lens' CreateDBClusterEndpointResponse Prelude.Int
createDBClusterEndpointResponse_httpStatus = Lens.lens (\CreateDBClusterEndpointResponse' {httpStatus} -> httpStatus) (\s@CreateDBClusterEndpointResponse' {} a -> s {httpStatus = a} :: CreateDBClusterEndpointResponse)

instance
  Prelude.NFData
    CreateDBClusterEndpointResponse
  where
  rnf CreateDBClusterEndpointResponse' {..} =
    Prelude.rnf dbClusterEndpointResourceIdentifier
      `Prelude.seq` Prelude.rnf staticMembers
      `Prelude.seq` Prelude.rnf dbClusterIdentifier
      `Prelude.seq` Prelude.rnf excludedMembers
      `Prelude.seq` Prelude.rnf customEndpointType
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf endpointType
      `Prelude.seq` Prelude.rnf dbClusterEndpointIdentifier
      `Prelude.seq` Prelude.rnf dbClusterEndpointArn
      `Prelude.seq` Prelude.rnf endpoint
      `Prelude.seq` Prelude.rnf httpStatus
