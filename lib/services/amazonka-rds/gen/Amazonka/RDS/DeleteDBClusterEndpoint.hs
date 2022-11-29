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
-- Module      : Amazonka.RDS.DeleteDBClusterEndpoint
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a custom endpoint and removes it from an Amazon Aurora DB
-- cluster.
--
-- This action only applies to Aurora DB clusters.
module Amazonka.RDS.DeleteDBClusterEndpoint
  ( -- * Creating a Request
    DeleteDBClusterEndpoint (..),
    newDeleteDBClusterEndpoint,

    -- * Request Lenses
    deleteDBClusterEndpoint_dbClusterEndpointIdentifier,

    -- * Destructuring the Response
    DBClusterEndpoint (..),
    newDBClusterEndpoint,

    -- * Response Lenses
    dbClusterEndpoint_dbClusterEndpointResourceIdentifier,
    dbClusterEndpoint_staticMembers,
    dbClusterEndpoint_dbClusterIdentifier,
    dbClusterEndpoint_excludedMembers,
    dbClusterEndpoint_customEndpointType,
    dbClusterEndpoint_status,
    dbClusterEndpoint_endpointType,
    dbClusterEndpoint_dbClusterEndpointIdentifier,
    dbClusterEndpoint_dbClusterEndpointArn,
    dbClusterEndpoint_endpoint,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteDBClusterEndpoint' smart constructor.
data DeleteDBClusterEndpoint = DeleteDBClusterEndpoint'
  { -- | The identifier associated with the custom endpoint. This parameter is
    -- stored as a lowercase string.
    dbClusterEndpointIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDBClusterEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbClusterEndpointIdentifier', 'deleteDBClusterEndpoint_dbClusterEndpointIdentifier' - The identifier associated with the custom endpoint. This parameter is
-- stored as a lowercase string.
newDeleteDBClusterEndpoint ::
  -- | 'dbClusterEndpointIdentifier'
  Prelude.Text ->
  DeleteDBClusterEndpoint
newDeleteDBClusterEndpoint
  pDBClusterEndpointIdentifier_ =
    DeleteDBClusterEndpoint'
      { dbClusterEndpointIdentifier =
          pDBClusterEndpointIdentifier_
      }

-- | The identifier associated with the custom endpoint. This parameter is
-- stored as a lowercase string.
deleteDBClusterEndpoint_dbClusterEndpointIdentifier :: Lens.Lens' DeleteDBClusterEndpoint Prelude.Text
deleteDBClusterEndpoint_dbClusterEndpointIdentifier = Lens.lens (\DeleteDBClusterEndpoint' {dbClusterEndpointIdentifier} -> dbClusterEndpointIdentifier) (\s@DeleteDBClusterEndpoint' {} a -> s {dbClusterEndpointIdentifier = a} :: DeleteDBClusterEndpoint)

instance Core.AWSRequest DeleteDBClusterEndpoint where
  type
    AWSResponse DeleteDBClusterEndpoint =
      DBClusterEndpoint
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DeleteDBClusterEndpointResult"
      (\s h x -> Core.parseXML x)

instance Prelude.Hashable DeleteDBClusterEndpoint where
  hashWithSalt _salt DeleteDBClusterEndpoint' {..} =
    _salt
      `Prelude.hashWithSalt` dbClusterEndpointIdentifier

instance Prelude.NFData DeleteDBClusterEndpoint where
  rnf DeleteDBClusterEndpoint' {..} =
    Prelude.rnf dbClusterEndpointIdentifier

instance Core.ToHeaders DeleteDBClusterEndpoint where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteDBClusterEndpoint where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteDBClusterEndpoint where
  toQuery DeleteDBClusterEndpoint' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DeleteDBClusterEndpoint" :: Prelude.ByteString),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "DBClusterEndpointIdentifier"
          Core.=: dbClusterEndpointIdentifier
      ]
