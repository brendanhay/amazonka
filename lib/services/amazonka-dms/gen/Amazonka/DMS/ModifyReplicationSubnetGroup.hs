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
-- Module      : Amazonka.DMS.ModifyReplicationSubnetGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the settings for the specified replication subnet group.
module Amazonka.DMS.ModifyReplicationSubnetGroup
  ( -- * Creating a Request
    ModifyReplicationSubnetGroup (..),
    newModifyReplicationSubnetGroup,

    -- * Request Lenses
    modifyReplicationSubnetGroup_replicationSubnetGroupDescription,
    modifyReplicationSubnetGroup_replicationSubnetGroupIdentifier,
    modifyReplicationSubnetGroup_subnetIds,

    -- * Destructuring the Response
    ModifyReplicationSubnetGroupResponse (..),
    newModifyReplicationSubnetGroupResponse,

    -- * Response Lenses
    modifyReplicationSubnetGroupResponse_replicationSubnetGroup,
    modifyReplicationSubnetGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newModifyReplicationSubnetGroup' smart constructor.
data ModifyReplicationSubnetGroup = ModifyReplicationSubnetGroup'
  { -- | A description for the replication instance subnet group.
    replicationSubnetGroupDescription :: Prelude.Maybe Prelude.Text,
    -- | The name of the replication instance subnet group.
    replicationSubnetGroupIdentifier :: Prelude.Text,
    -- | A list of subnet IDs.
    subnetIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyReplicationSubnetGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationSubnetGroupDescription', 'modifyReplicationSubnetGroup_replicationSubnetGroupDescription' - A description for the replication instance subnet group.
--
-- 'replicationSubnetGroupIdentifier', 'modifyReplicationSubnetGroup_replicationSubnetGroupIdentifier' - The name of the replication instance subnet group.
--
-- 'subnetIds', 'modifyReplicationSubnetGroup_subnetIds' - A list of subnet IDs.
newModifyReplicationSubnetGroup ::
  -- | 'replicationSubnetGroupIdentifier'
  Prelude.Text ->
  ModifyReplicationSubnetGroup
newModifyReplicationSubnetGroup
  pReplicationSubnetGroupIdentifier_ =
    ModifyReplicationSubnetGroup'
      { replicationSubnetGroupDescription =
          Prelude.Nothing,
        replicationSubnetGroupIdentifier =
          pReplicationSubnetGroupIdentifier_,
        subnetIds = Prelude.mempty
      }

-- | A description for the replication instance subnet group.
modifyReplicationSubnetGroup_replicationSubnetGroupDescription :: Lens.Lens' ModifyReplicationSubnetGroup (Prelude.Maybe Prelude.Text)
modifyReplicationSubnetGroup_replicationSubnetGroupDescription = Lens.lens (\ModifyReplicationSubnetGroup' {replicationSubnetGroupDescription} -> replicationSubnetGroupDescription) (\s@ModifyReplicationSubnetGroup' {} a -> s {replicationSubnetGroupDescription = a} :: ModifyReplicationSubnetGroup)

-- | The name of the replication instance subnet group.
modifyReplicationSubnetGroup_replicationSubnetGroupIdentifier :: Lens.Lens' ModifyReplicationSubnetGroup Prelude.Text
modifyReplicationSubnetGroup_replicationSubnetGroupIdentifier = Lens.lens (\ModifyReplicationSubnetGroup' {replicationSubnetGroupIdentifier} -> replicationSubnetGroupIdentifier) (\s@ModifyReplicationSubnetGroup' {} a -> s {replicationSubnetGroupIdentifier = a} :: ModifyReplicationSubnetGroup)

-- | A list of subnet IDs.
modifyReplicationSubnetGroup_subnetIds :: Lens.Lens' ModifyReplicationSubnetGroup [Prelude.Text]
modifyReplicationSubnetGroup_subnetIds = Lens.lens (\ModifyReplicationSubnetGroup' {subnetIds} -> subnetIds) (\s@ModifyReplicationSubnetGroup' {} a -> s {subnetIds = a} :: ModifyReplicationSubnetGroup) Prelude.. Lens.coerced

instance Core.AWSRequest ModifyReplicationSubnetGroup where
  type
    AWSResponse ModifyReplicationSubnetGroup =
      ModifyReplicationSubnetGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ModifyReplicationSubnetGroupResponse'
            Prelude.<$> (x Data..?> "ReplicationSubnetGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ModifyReplicationSubnetGroup
  where
  hashWithSalt _salt ModifyReplicationSubnetGroup' {..} =
    _salt
      `Prelude.hashWithSalt` replicationSubnetGroupDescription
      `Prelude.hashWithSalt` replicationSubnetGroupIdentifier
      `Prelude.hashWithSalt` subnetIds

instance Prelude.NFData ModifyReplicationSubnetGroup where
  rnf ModifyReplicationSubnetGroup' {..} =
    Prelude.rnf replicationSubnetGroupDescription
      `Prelude.seq` Prelude.rnf replicationSubnetGroupIdentifier
      `Prelude.seq` Prelude.rnf subnetIds

instance Data.ToHeaders ModifyReplicationSubnetGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonDMSv20160101.ModifyReplicationSubnetGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ModifyReplicationSubnetGroup where
  toJSON ModifyReplicationSubnetGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ReplicationSubnetGroupDescription" Data..=)
              Prelude.<$> replicationSubnetGroupDescription,
            Prelude.Just
              ( "ReplicationSubnetGroupIdentifier"
                  Data..= replicationSubnetGroupIdentifier
              ),
            Prelude.Just ("SubnetIds" Data..= subnetIds)
          ]
      )

instance Data.ToPath ModifyReplicationSubnetGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyReplicationSubnetGroup where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newModifyReplicationSubnetGroupResponse' smart constructor.
data ModifyReplicationSubnetGroupResponse = ModifyReplicationSubnetGroupResponse'
  { -- | The modified replication subnet group.
    replicationSubnetGroup :: Prelude.Maybe ReplicationSubnetGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyReplicationSubnetGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationSubnetGroup', 'modifyReplicationSubnetGroupResponse_replicationSubnetGroup' - The modified replication subnet group.
--
-- 'httpStatus', 'modifyReplicationSubnetGroupResponse_httpStatus' - The response's http status code.
newModifyReplicationSubnetGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyReplicationSubnetGroupResponse
newModifyReplicationSubnetGroupResponse pHttpStatus_ =
  ModifyReplicationSubnetGroupResponse'
    { replicationSubnetGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The modified replication subnet group.
modifyReplicationSubnetGroupResponse_replicationSubnetGroup :: Lens.Lens' ModifyReplicationSubnetGroupResponse (Prelude.Maybe ReplicationSubnetGroup)
modifyReplicationSubnetGroupResponse_replicationSubnetGroup = Lens.lens (\ModifyReplicationSubnetGroupResponse' {replicationSubnetGroup} -> replicationSubnetGroup) (\s@ModifyReplicationSubnetGroupResponse' {} a -> s {replicationSubnetGroup = a} :: ModifyReplicationSubnetGroupResponse)

-- | The response's http status code.
modifyReplicationSubnetGroupResponse_httpStatus :: Lens.Lens' ModifyReplicationSubnetGroupResponse Prelude.Int
modifyReplicationSubnetGroupResponse_httpStatus = Lens.lens (\ModifyReplicationSubnetGroupResponse' {httpStatus} -> httpStatus) (\s@ModifyReplicationSubnetGroupResponse' {} a -> s {httpStatus = a} :: ModifyReplicationSubnetGroupResponse)

instance
  Prelude.NFData
    ModifyReplicationSubnetGroupResponse
  where
  rnf ModifyReplicationSubnetGroupResponse' {..} =
    Prelude.rnf replicationSubnetGroup
      `Prelude.seq` Prelude.rnf httpStatus
