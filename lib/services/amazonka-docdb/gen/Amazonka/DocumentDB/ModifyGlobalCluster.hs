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
-- Module      : Amazonka.DocumentDB.ModifyGlobalCluster
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modify a setting for an Amazon DocumentDB global cluster. You can change
-- one or more configuration parameters (for example: deletion protection),
-- or the global cluster identifier by specifying these parameters and the
-- new values in the request.
--
-- This action only applies to Amazon DocumentDB clusters.
module Amazonka.DocumentDB.ModifyGlobalCluster
  ( -- * Creating a Request
    ModifyGlobalCluster (..),
    newModifyGlobalCluster,

    -- * Request Lenses
    modifyGlobalCluster_deletionProtection,
    modifyGlobalCluster_newGlobalClusterIdentifier,
    modifyGlobalCluster_globalClusterIdentifier,

    -- * Destructuring the Response
    ModifyGlobalClusterResponse (..),
    newModifyGlobalClusterResponse,

    -- * Response Lenses
    modifyGlobalClusterResponse_globalCluster,
    modifyGlobalClusterResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DocumentDB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input to ModifyGlobalCluster.
--
-- /See:/ 'newModifyGlobalCluster' smart constructor.
data ModifyGlobalCluster = ModifyGlobalCluster'
  { -- | Indicates if the global cluster has deletion protection enabled. The
    -- global cluster can\'t be deleted when deletion protection is enabled.
    deletionProtection :: Prelude.Maybe Prelude.Bool,
    -- | The new identifier for a global cluster when you modify a global
    -- cluster. This value is stored as a lowercase string.
    --
    -- -   Must contain from 1 to 63 letters, numbers, or hyphens
    --
    --     The first character must be a letter
    --
    --     Can\'t end with a hyphen or contain two consecutive hyphens
    --
    -- Example: @my-cluster2@
    newGlobalClusterIdentifier' :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the global cluster being modified. This parameter
    -- isn\'t case-sensitive.
    --
    -- Constraints:
    --
    -- -   Must match the identifier of an existing global cluster.
    globalClusterIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyGlobalCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deletionProtection', 'modifyGlobalCluster_deletionProtection' - Indicates if the global cluster has deletion protection enabled. The
-- global cluster can\'t be deleted when deletion protection is enabled.
--
-- 'newGlobalClusterIdentifier'', 'modifyGlobalCluster_newGlobalClusterIdentifier' - The new identifier for a global cluster when you modify a global
-- cluster. This value is stored as a lowercase string.
--
-- -   Must contain from 1 to 63 letters, numbers, or hyphens
--
--     The first character must be a letter
--
--     Can\'t end with a hyphen or contain two consecutive hyphens
--
-- Example: @my-cluster2@
--
-- 'globalClusterIdentifier', 'modifyGlobalCluster_globalClusterIdentifier' - The identifier for the global cluster being modified. This parameter
-- isn\'t case-sensitive.
--
-- Constraints:
--
-- -   Must match the identifier of an existing global cluster.
newModifyGlobalCluster ::
  -- | 'globalClusterIdentifier'
  Prelude.Text ->
  ModifyGlobalCluster
newModifyGlobalCluster pGlobalClusterIdentifier_ =
  ModifyGlobalCluster'
    { deletionProtection =
        Prelude.Nothing,
      newGlobalClusterIdentifier' = Prelude.Nothing,
      globalClusterIdentifier = pGlobalClusterIdentifier_
    }

-- | Indicates if the global cluster has deletion protection enabled. The
-- global cluster can\'t be deleted when deletion protection is enabled.
modifyGlobalCluster_deletionProtection :: Lens.Lens' ModifyGlobalCluster (Prelude.Maybe Prelude.Bool)
modifyGlobalCluster_deletionProtection = Lens.lens (\ModifyGlobalCluster' {deletionProtection} -> deletionProtection) (\s@ModifyGlobalCluster' {} a -> s {deletionProtection = a} :: ModifyGlobalCluster)

-- | The new identifier for a global cluster when you modify a global
-- cluster. This value is stored as a lowercase string.
--
-- -   Must contain from 1 to 63 letters, numbers, or hyphens
--
--     The first character must be a letter
--
--     Can\'t end with a hyphen or contain two consecutive hyphens
--
-- Example: @my-cluster2@
modifyGlobalCluster_newGlobalClusterIdentifier :: Lens.Lens' ModifyGlobalCluster (Prelude.Maybe Prelude.Text)
modifyGlobalCluster_newGlobalClusterIdentifier = Lens.lens (\ModifyGlobalCluster' {newGlobalClusterIdentifier'} -> newGlobalClusterIdentifier') (\s@ModifyGlobalCluster' {} a -> s {newGlobalClusterIdentifier' = a} :: ModifyGlobalCluster)

-- | The identifier for the global cluster being modified. This parameter
-- isn\'t case-sensitive.
--
-- Constraints:
--
-- -   Must match the identifier of an existing global cluster.
modifyGlobalCluster_globalClusterIdentifier :: Lens.Lens' ModifyGlobalCluster Prelude.Text
modifyGlobalCluster_globalClusterIdentifier = Lens.lens (\ModifyGlobalCluster' {globalClusterIdentifier} -> globalClusterIdentifier) (\s@ModifyGlobalCluster' {} a -> s {globalClusterIdentifier = a} :: ModifyGlobalCluster)

instance Core.AWSRequest ModifyGlobalCluster where
  type
    AWSResponse ModifyGlobalCluster =
      ModifyGlobalClusterResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ModifyGlobalClusterResult"
      ( \s h x ->
          ModifyGlobalClusterResponse'
            Prelude.<$> (x Core..@? "GlobalCluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyGlobalCluster where
  hashWithSalt _salt ModifyGlobalCluster' {..} =
    _salt `Prelude.hashWithSalt` deletionProtection
      `Prelude.hashWithSalt` newGlobalClusterIdentifier'
      `Prelude.hashWithSalt` globalClusterIdentifier

instance Prelude.NFData ModifyGlobalCluster where
  rnf ModifyGlobalCluster' {..} =
    Prelude.rnf deletionProtection
      `Prelude.seq` Prelude.rnf newGlobalClusterIdentifier'
      `Prelude.seq` Prelude.rnf globalClusterIdentifier

instance Core.ToHeaders ModifyGlobalCluster where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ModifyGlobalCluster where
  toPath = Prelude.const "/"

instance Core.ToQuery ModifyGlobalCluster where
  toQuery ModifyGlobalCluster' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ModifyGlobalCluster" :: Prelude.ByteString),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "DeletionProtection" Core.=: deletionProtection,
        "NewGlobalClusterIdentifier"
          Core.=: newGlobalClusterIdentifier',
        "GlobalClusterIdentifier"
          Core.=: globalClusterIdentifier
      ]

-- | /See:/ 'newModifyGlobalClusterResponse' smart constructor.
data ModifyGlobalClusterResponse = ModifyGlobalClusterResponse'
  { globalCluster :: Prelude.Maybe GlobalCluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyGlobalClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalCluster', 'modifyGlobalClusterResponse_globalCluster' - Undocumented member.
--
-- 'httpStatus', 'modifyGlobalClusterResponse_httpStatus' - The response's http status code.
newModifyGlobalClusterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyGlobalClusterResponse
newModifyGlobalClusterResponse pHttpStatus_ =
  ModifyGlobalClusterResponse'
    { globalCluster =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
modifyGlobalClusterResponse_globalCluster :: Lens.Lens' ModifyGlobalClusterResponse (Prelude.Maybe GlobalCluster)
modifyGlobalClusterResponse_globalCluster = Lens.lens (\ModifyGlobalClusterResponse' {globalCluster} -> globalCluster) (\s@ModifyGlobalClusterResponse' {} a -> s {globalCluster = a} :: ModifyGlobalClusterResponse)

-- | The response's http status code.
modifyGlobalClusterResponse_httpStatus :: Lens.Lens' ModifyGlobalClusterResponse Prelude.Int
modifyGlobalClusterResponse_httpStatus = Lens.lens (\ModifyGlobalClusterResponse' {httpStatus} -> httpStatus) (\s@ModifyGlobalClusterResponse' {} a -> s {httpStatus = a} :: ModifyGlobalClusterResponse)

instance Prelude.NFData ModifyGlobalClusterResponse where
  rnf ModifyGlobalClusterResponse' {..} =
    Prelude.rnf globalCluster
      `Prelude.seq` Prelude.rnf httpStatus
