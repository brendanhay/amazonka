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
-- Module      : Amazonka.DMS.CreateReplicationSubnetGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a replication subnet group given a list of the subnet IDs in a
-- VPC.
--
-- The VPC needs to have at least one subnet in at least two availability
-- zones in the Amazon Web Services Region, otherwise the service will
-- throw a @ReplicationSubnetGroupDoesNotCoverEnoughAZs@ exception.
module Amazonka.DMS.CreateReplicationSubnetGroup
  ( -- * Creating a Request
    CreateReplicationSubnetGroup (..),
    newCreateReplicationSubnetGroup,

    -- * Request Lenses
    createReplicationSubnetGroup_tags,
    createReplicationSubnetGroup_replicationSubnetGroupIdentifier,
    createReplicationSubnetGroup_replicationSubnetGroupDescription,
    createReplicationSubnetGroup_subnetIds,

    -- * Destructuring the Response
    CreateReplicationSubnetGroupResponse (..),
    newCreateReplicationSubnetGroupResponse,

    -- * Response Lenses
    createReplicationSubnetGroupResponse_replicationSubnetGroup,
    createReplicationSubnetGroupResponse_httpStatus,
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
-- /See:/ 'newCreateReplicationSubnetGroup' smart constructor.
data CreateReplicationSubnetGroup = CreateReplicationSubnetGroup'
  { -- | One or more tags to be assigned to the subnet group.
    tags :: Prelude.Maybe [Tag],
    -- | The name for the replication subnet group. This value is stored as a
    -- lowercase string.
    --
    -- Constraints: Must contain no more than 255 alphanumeric characters,
    -- periods, spaces, underscores, or hyphens. Must not be \"default\".
    --
    -- Example: @mySubnetgroup@
    replicationSubnetGroupIdentifier :: Prelude.Text,
    -- | The description for the subnet group.
    replicationSubnetGroupDescription :: Prelude.Text,
    -- | One or more subnet IDs to be assigned to the subnet group.
    subnetIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateReplicationSubnetGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createReplicationSubnetGroup_tags' - One or more tags to be assigned to the subnet group.
--
-- 'replicationSubnetGroupIdentifier', 'createReplicationSubnetGroup_replicationSubnetGroupIdentifier' - The name for the replication subnet group. This value is stored as a
-- lowercase string.
--
-- Constraints: Must contain no more than 255 alphanumeric characters,
-- periods, spaces, underscores, or hyphens. Must not be \"default\".
--
-- Example: @mySubnetgroup@
--
-- 'replicationSubnetGroupDescription', 'createReplicationSubnetGroup_replicationSubnetGroupDescription' - The description for the subnet group.
--
-- 'subnetIds', 'createReplicationSubnetGroup_subnetIds' - One or more subnet IDs to be assigned to the subnet group.
newCreateReplicationSubnetGroup ::
  -- | 'replicationSubnetGroupIdentifier'
  Prelude.Text ->
  -- | 'replicationSubnetGroupDescription'
  Prelude.Text ->
  CreateReplicationSubnetGroup
newCreateReplicationSubnetGroup
  pReplicationSubnetGroupIdentifier_
  pReplicationSubnetGroupDescription_ =
    CreateReplicationSubnetGroup'
      { tags =
          Prelude.Nothing,
        replicationSubnetGroupIdentifier =
          pReplicationSubnetGroupIdentifier_,
        replicationSubnetGroupDescription =
          pReplicationSubnetGroupDescription_,
        subnetIds = Prelude.mempty
      }

-- | One or more tags to be assigned to the subnet group.
createReplicationSubnetGroup_tags :: Lens.Lens' CreateReplicationSubnetGroup (Prelude.Maybe [Tag])
createReplicationSubnetGroup_tags = Lens.lens (\CreateReplicationSubnetGroup' {tags} -> tags) (\s@CreateReplicationSubnetGroup' {} a -> s {tags = a} :: CreateReplicationSubnetGroup) Prelude.. Lens.mapping Lens.coerced

-- | The name for the replication subnet group. This value is stored as a
-- lowercase string.
--
-- Constraints: Must contain no more than 255 alphanumeric characters,
-- periods, spaces, underscores, or hyphens. Must not be \"default\".
--
-- Example: @mySubnetgroup@
createReplicationSubnetGroup_replicationSubnetGroupIdentifier :: Lens.Lens' CreateReplicationSubnetGroup Prelude.Text
createReplicationSubnetGroup_replicationSubnetGroupIdentifier = Lens.lens (\CreateReplicationSubnetGroup' {replicationSubnetGroupIdentifier} -> replicationSubnetGroupIdentifier) (\s@CreateReplicationSubnetGroup' {} a -> s {replicationSubnetGroupIdentifier = a} :: CreateReplicationSubnetGroup)

-- | The description for the subnet group.
createReplicationSubnetGroup_replicationSubnetGroupDescription :: Lens.Lens' CreateReplicationSubnetGroup Prelude.Text
createReplicationSubnetGroup_replicationSubnetGroupDescription = Lens.lens (\CreateReplicationSubnetGroup' {replicationSubnetGroupDescription} -> replicationSubnetGroupDescription) (\s@CreateReplicationSubnetGroup' {} a -> s {replicationSubnetGroupDescription = a} :: CreateReplicationSubnetGroup)

-- | One or more subnet IDs to be assigned to the subnet group.
createReplicationSubnetGroup_subnetIds :: Lens.Lens' CreateReplicationSubnetGroup [Prelude.Text]
createReplicationSubnetGroup_subnetIds = Lens.lens (\CreateReplicationSubnetGroup' {subnetIds} -> subnetIds) (\s@CreateReplicationSubnetGroup' {} a -> s {subnetIds = a} :: CreateReplicationSubnetGroup) Prelude.. Lens.coerced

instance Core.AWSRequest CreateReplicationSubnetGroup where
  type
    AWSResponse CreateReplicationSubnetGroup =
      CreateReplicationSubnetGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateReplicationSubnetGroupResponse'
            Prelude.<$> (x Data..?> "ReplicationSubnetGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateReplicationSubnetGroup
  where
  hashWithSalt _salt CreateReplicationSubnetGroup' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` replicationSubnetGroupIdentifier
      `Prelude.hashWithSalt` replicationSubnetGroupDescription
      `Prelude.hashWithSalt` subnetIds

instance Prelude.NFData CreateReplicationSubnetGroup where
  rnf CreateReplicationSubnetGroup' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf replicationSubnetGroupIdentifier
      `Prelude.seq` Prelude.rnf replicationSubnetGroupDescription
      `Prelude.seq` Prelude.rnf subnetIds

instance Data.ToHeaders CreateReplicationSubnetGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonDMSv20160101.CreateReplicationSubnetGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateReplicationSubnetGroup where
  toJSON CreateReplicationSubnetGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ( "ReplicationSubnetGroupIdentifier"
                  Data..= replicationSubnetGroupIdentifier
              ),
            Prelude.Just
              ( "ReplicationSubnetGroupDescription"
                  Data..= replicationSubnetGroupDescription
              ),
            Prelude.Just ("SubnetIds" Data..= subnetIds)
          ]
      )

instance Data.ToPath CreateReplicationSubnetGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateReplicationSubnetGroup where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newCreateReplicationSubnetGroupResponse' smart constructor.
data CreateReplicationSubnetGroupResponse = CreateReplicationSubnetGroupResponse'
  { -- | The replication subnet group that was created.
    replicationSubnetGroup :: Prelude.Maybe ReplicationSubnetGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateReplicationSubnetGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationSubnetGroup', 'createReplicationSubnetGroupResponse_replicationSubnetGroup' - The replication subnet group that was created.
--
-- 'httpStatus', 'createReplicationSubnetGroupResponse_httpStatus' - The response's http status code.
newCreateReplicationSubnetGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateReplicationSubnetGroupResponse
newCreateReplicationSubnetGroupResponse pHttpStatus_ =
  CreateReplicationSubnetGroupResponse'
    { replicationSubnetGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The replication subnet group that was created.
createReplicationSubnetGroupResponse_replicationSubnetGroup :: Lens.Lens' CreateReplicationSubnetGroupResponse (Prelude.Maybe ReplicationSubnetGroup)
createReplicationSubnetGroupResponse_replicationSubnetGroup = Lens.lens (\CreateReplicationSubnetGroupResponse' {replicationSubnetGroup} -> replicationSubnetGroup) (\s@CreateReplicationSubnetGroupResponse' {} a -> s {replicationSubnetGroup = a} :: CreateReplicationSubnetGroupResponse)

-- | The response's http status code.
createReplicationSubnetGroupResponse_httpStatus :: Lens.Lens' CreateReplicationSubnetGroupResponse Prelude.Int
createReplicationSubnetGroupResponse_httpStatus = Lens.lens (\CreateReplicationSubnetGroupResponse' {httpStatus} -> httpStatus) (\s@CreateReplicationSubnetGroupResponse' {} a -> s {httpStatus = a} :: CreateReplicationSubnetGroupResponse)

instance
  Prelude.NFData
    CreateReplicationSubnetGroupResponse
  where
  rnf CreateReplicationSubnetGroupResponse' {..} =
    Prelude.rnf replicationSubnetGroup
      `Prelude.seq` Prelude.rnf httpStatus
