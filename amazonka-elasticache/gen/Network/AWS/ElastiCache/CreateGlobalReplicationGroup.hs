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
-- Module      : Network.AWS.ElastiCache.CreateGlobalReplicationGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Global Datastore for Redis offers fully managed, fast, reliable and
-- secure cross-region replication. Using Global Datastore for Redis, you
-- can create cross-region read replica clusters for ElastiCache for Redis
-- to enable low-latency reads and disaster recovery across regions. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Redis-Global-Datastore.html Replication Across Regions Using Global Datastore>.
--
-- -   The __GlobalReplicationGroupIdSuffix__ is the name of the Global
--     Datastore.
--
-- -   The __PrimaryReplicationGroupId__ represents the name of the primary
--     cluster that accepts writes and will replicate updates to the
--     secondary cluster.
module Network.AWS.ElastiCache.CreateGlobalReplicationGroup
  ( -- * Creating a Request
    CreateGlobalReplicationGroup (..),
    newCreateGlobalReplicationGroup,

    -- * Request Lenses
    createGlobalReplicationGroup_globalReplicationGroupDescription,
    createGlobalReplicationGroup_globalReplicationGroupIdSuffix,
    createGlobalReplicationGroup_primaryReplicationGroupId,

    -- * Destructuring the Response
    CreateGlobalReplicationGroupResponse (..),
    newCreateGlobalReplicationGroupResponse,

    -- * Response Lenses
    createGlobalReplicationGroupResponse_globalReplicationGroup,
    createGlobalReplicationGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateGlobalReplicationGroup' smart constructor.
data CreateGlobalReplicationGroup = CreateGlobalReplicationGroup'
  { -- | Provides details of the Global Datastore
    globalReplicationGroupDescription :: Core.Maybe Core.Text,
    -- | The suffix name of a Global Datastore. Amazon ElastiCache automatically
    -- applies a prefix to the Global Datastore ID when it is created. Each AWS
    -- Region has its own prefix. For instance, a Global Datastore ID created
    -- in the US-West-1 region will begin with \"dsdfu\" along with the suffix
    -- name you provide. The suffix, combined with the auto-generated prefix,
    -- guarantees uniqueness of the Global Datastore name across multiple
    -- regions.
    --
    -- For a full list of AWS Regions and their respective Global Datastore iD
    -- prefixes, see
    -- <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Redis-Global-Datastores-CLI.html Using the AWS CLI with Global Datastores>
    -- .
    globalReplicationGroupIdSuffix :: Core.Text,
    -- | The name of the primary cluster that accepts writes and will replicate
    -- updates to the secondary cluster.
    primaryReplicationGroupId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateGlobalReplicationGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalReplicationGroupDescription', 'createGlobalReplicationGroup_globalReplicationGroupDescription' - Provides details of the Global Datastore
--
-- 'globalReplicationGroupIdSuffix', 'createGlobalReplicationGroup_globalReplicationGroupIdSuffix' - The suffix name of a Global Datastore. Amazon ElastiCache automatically
-- applies a prefix to the Global Datastore ID when it is created. Each AWS
-- Region has its own prefix. For instance, a Global Datastore ID created
-- in the US-West-1 region will begin with \"dsdfu\" along with the suffix
-- name you provide. The suffix, combined with the auto-generated prefix,
-- guarantees uniqueness of the Global Datastore name across multiple
-- regions.
--
-- For a full list of AWS Regions and their respective Global Datastore iD
-- prefixes, see
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Redis-Global-Datastores-CLI.html Using the AWS CLI with Global Datastores>
-- .
--
-- 'primaryReplicationGroupId', 'createGlobalReplicationGroup_primaryReplicationGroupId' - The name of the primary cluster that accepts writes and will replicate
-- updates to the secondary cluster.
newCreateGlobalReplicationGroup ::
  -- | 'globalReplicationGroupIdSuffix'
  Core.Text ->
  -- | 'primaryReplicationGroupId'
  Core.Text ->
  CreateGlobalReplicationGroup
newCreateGlobalReplicationGroup
  pGlobalReplicationGroupIdSuffix_
  pPrimaryReplicationGroupId_ =
    CreateGlobalReplicationGroup'
      { globalReplicationGroupDescription =
          Core.Nothing,
        globalReplicationGroupIdSuffix =
          pGlobalReplicationGroupIdSuffix_,
        primaryReplicationGroupId =
          pPrimaryReplicationGroupId_
      }

-- | Provides details of the Global Datastore
createGlobalReplicationGroup_globalReplicationGroupDescription :: Lens.Lens' CreateGlobalReplicationGroup (Core.Maybe Core.Text)
createGlobalReplicationGroup_globalReplicationGroupDescription = Lens.lens (\CreateGlobalReplicationGroup' {globalReplicationGroupDescription} -> globalReplicationGroupDescription) (\s@CreateGlobalReplicationGroup' {} a -> s {globalReplicationGroupDescription = a} :: CreateGlobalReplicationGroup)

-- | The suffix name of a Global Datastore. Amazon ElastiCache automatically
-- applies a prefix to the Global Datastore ID when it is created. Each AWS
-- Region has its own prefix. For instance, a Global Datastore ID created
-- in the US-West-1 region will begin with \"dsdfu\" along with the suffix
-- name you provide. The suffix, combined with the auto-generated prefix,
-- guarantees uniqueness of the Global Datastore name across multiple
-- regions.
--
-- For a full list of AWS Regions and their respective Global Datastore iD
-- prefixes, see
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Redis-Global-Datastores-CLI.html Using the AWS CLI with Global Datastores>
-- .
createGlobalReplicationGroup_globalReplicationGroupIdSuffix :: Lens.Lens' CreateGlobalReplicationGroup Core.Text
createGlobalReplicationGroup_globalReplicationGroupIdSuffix = Lens.lens (\CreateGlobalReplicationGroup' {globalReplicationGroupIdSuffix} -> globalReplicationGroupIdSuffix) (\s@CreateGlobalReplicationGroup' {} a -> s {globalReplicationGroupIdSuffix = a} :: CreateGlobalReplicationGroup)

-- | The name of the primary cluster that accepts writes and will replicate
-- updates to the secondary cluster.
createGlobalReplicationGroup_primaryReplicationGroupId :: Lens.Lens' CreateGlobalReplicationGroup Core.Text
createGlobalReplicationGroup_primaryReplicationGroupId = Lens.lens (\CreateGlobalReplicationGroup' {primaryReplicationGroupId} -> primaryReplicationGroupId) (\s@CreateGlobalReplicationGroup' {} a -> s {primaryReplicationGroupId = a} :: CreateGlobalReplicationGroup)

instance Core.AWSRequest CreateGlobalReplicationGroup where
  type
    AWSResponse CreateGlobalReplicationGroup =
      CreateGlobalReplicationGroupResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CreateGlobalReplicationGroupResult"
      ( \s h x ->
          CreateGlobalReplicationGroupResponse'
            Core.<$> (x Core..@? "GlobalReplicationGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateGlobalReplicationGroup

instance Core.NFData CreateGlobalReplicationGroup

instance Core.ToHeaders CreateGlobalReplicationGroup where
  toHeaders = Core.const Core.mempty

instance Core.ToPath CreateGlobalReplicationGroup where
  toPath = Core.const "/"

instance Core.ToQuery CreateGlobalReplicationGroup where
  toQuery CreateGlobalReplicationGroup' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("CreateGlobalReplicationGroup" :: Core.ByteString),
        "Version" Core.=: ("2015-02-02" :: Core.ByteString),
        "GlobalReplicationGroupDescription"
          Core.=: globalReplicationGroupDescription,
        "GlobalReplicationGroupIdSuffix"
          Core.=: globalReplicationGroupIdSuffix,
        "PrimaryReplicationGroupId"
          Core.=: primaryReplicationGroupId
      ]

-- | /See:/ 'newCreateGlobalReplicationGroupResponse' smart constructor.
data CreateGlobalReplicationGroupResponse = CreateGlobalReplicationGroupResponse'
  { globalReplicationGroup :: Core.Maybe GlobalReplicationGroup,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateGlobalReplicationGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalReplicationGroup', 'createGlobalReplicationGroupResponse_globalReplicationGroup' - Undocumented member.
--
-- 'httpStatus', 'createGlobalReplicationGroupResponse_httpStatus' - The response's http status code.
newCreateGlobalReplicationGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateGlobalReplicationGroupResponse
newCreateGlobalReplicationGroupResponse pHttpStatus_ =
  CreateGlobalReplicationGroupResponse'
    { globalReplicationGroup =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createGlobalReplicationGroupResponse_globalReplicationGroup :: Lens.Lens' CreateGlobalReplicationGroupResponse (Core.Maybe GlobalReplicationGroup)
createGlobalReplicationGroupResponse_globalReplicationGroup = Lens.lens (\CreateGlobalReplicationGroupResponse' {globalReplicationGroup} -> globalReplicationGroup) (\s@CreateGlobalReplicationGroupResponse' {} a -> s {globalReplicationGroup = a} :: CreateGlobalReplicationGroupResponse)

-- | The response's http status code.
createGlobalReplicationGroupResponse_httpStatus :: Lens.Lens' CreateGlobalReplicationGroupResponse Core.Int
createGlobalReplicationGroupResponse_httpStatus = Lens.lens (\CreateGlobalReplicationGroupResponse' {httpStatus} -> httpStatus) (\s@CreateGlobalReplicationGroupResponse' {} a -> s {httpStatus = a} :: CreateGlobalReplicationGroupResponse)

instance
  Core.NFData
    CreateGlobalReplicationGroupResponse
