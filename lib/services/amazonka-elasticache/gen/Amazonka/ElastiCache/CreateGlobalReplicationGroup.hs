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
-- Module      : Amazonka.ElastiCache.CreateGlobalReplicationGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
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
--     datastore.
--
-- -   The __PrimaryReplicationGroupId__ represents the name of the primary
--     cluster that accepts writes and will replicate updates to the
--     secondary cluster.
module Amazonka.ElastiCache.CreateGlobalReplicationGroup
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ElastiCache.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateGlobalReplicationGroup' smart constructor.
data CreateGlobalReplicationGroup = CreateGlobalReplicationGroup'
  { -- | Provides details of the Global datastore
    globalReplicationGroupDescription :: Prelude.Maybe Prelude.Text,
    -- | The suffix name of a Global datastore. Amazon ElastiCache automatically
    -- applies a prefix to the Global datastore ID when it is created. Each
    -- Amazon Region has its own prefix. For instance, a Global datastore ID
    -- created in the US-West-1 region will begin with \"dsdfu\" along with the
    -- suffix name you provide. The suffix, combined with the auto-generated
    -- prefix, guarantees uniqueness of the Global datastore name across
    -- multiple regions.
    --
    -- For a full list of Amazon Regions and their respective Global datastore
    -- iD prefixes, see
    -- <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Redis-Global-Datastores-CLI.html Using the Amazon CLI with Global datastores>
    -- .
    globalReplicationGroupIdSuffix :: Prelude.Text,
    -- | The name of the primary cluster that accepts writes and will replicate
    -- updates to the secondary cluster.
    primaryReplicationGroupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateGlobalReplicationGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalReplicationGroupDescription', 'createGlobalReplicationGroup_globalReplicationGroupDescription' - Provides details of the Global datastore
--
-- 'globalReplicationGroupIdSuffix', 'createGlobalReplicationGroup_globalReplicationGroupIdSuffix' - The suffix name of a Global datastore. Amazon ElastiCache automatically
-- applies a prefix to the Global datastore ID when it is created. Each
-- Amazon Region has its own prefix. For instance, a Global datastore ID
-- created in the US-West-1 region will begin with \"dsdfu\" along with the
-- suffix name you provide. The suffix, combined with the auto-generated
-- prefix, guarantees uniqueness of the Global datastore name across
-- multiple regions.
--
-- For a full list of Amazon Regions and their respective Global datastore
-- iD prefixes, see
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Redis-Global-Datastores-CLI.html Using the Amazon CLI with Global datastores>
-- .
--
-- 'primaryReplicationGroupId', 'createGlobalReplicationGroup_primaryReplicationGroupId' - The name of the primary cluster that accepts writes and will replicate
-- updates to the secondary cluster.
newCreateGlobalReplicationGroup ::
  -- | 'globalReplicationGroupIdSuffix'
  Prelude.Text ->
  -- | 'primaryReplicationGroupId'
  Prelude.Text ->
  CreateGlobalReplicationGroup
newCreateGlobalReplicationGroup
  pGlobalReplicationGroupIdSuffix_
  pPrimaryReplicationGroupId_ =
    CreateGlobalReplicationGroup'
      { globalReplicationGroupDescription =
          Prelude.Nothing,
        globalReplicationGroupIdSuffix =
          pGlobalReplicationGroupIdSuffix_,
        primaryReplicationGroupId =
          pPrimaryReplicationGroupId_
      }

-- | Provides details of the Global datastore
createGlobalReplicationGroup_globalReplicationGroupDescription :: Lens.Lens' CreateGlobalReplicationGroup (Prelude.Maybe Prelude.Text)
createGlobalReplicationGroup_globalReplicationGroupDescription = Lens.lens (\CreateGlobalReplicationGroup' {globalReplicationGroupDescription} -> globalReplicationGroupDescription) (\s@CreateGlobalReplicationGroup' {} a -> s {globalReplicationGroupDescription = a} :: CreateGlobalReplicationGroup)

-- | The suffix name of a Global datastore. Amazon ElastiCache automatically
-- applies a prefix to the Global datastore ID when it is created. Each
-- Amazon Region has its own prefix. For instance, a Global datastore ID
-- created in the US-West-1 region will begin with \"dsdfu\" along with the
-- suffix name you provide. The suffix, combined with the auto-generated
-- prefix, guarantees uniqueness of the Global datastore name across
-- multiple regions.
--
-- For a full list of Amazon Regions and their respective Global datastore
-- iD prefixes, see
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Redis-Global-Datastores-CLI.html Using the Amazon CLI with Global datastores>
-- .
createGlobalReplicationGroup_globalReplicationGroupIdSuffix :: Lens.Lens' CreateGlobalReplicationGroup Prelude.Text
createGlobalReplicationGroup_globalReplicationGroupIdSuffix = Lens.lens (\CreateGlobalReplicationGroup' {globalReplicationGroupIdSuffix} -> globalReplicationGroupIdSuffix) (\s@CreateGlobalReplicationGroup' {} a -> s {globalReplicationGroupIdSuffix = a} :: CreateGlobalReplicationGroup)

-- | The name of the primary cluster that accepts writes and will replicate
-- updates to the secondary cluster.
createGlobalReplicationGroup_primaryReplicationGroupId :: Lens.Lens' CreateGlobalReplicationGroup Prelude.Text
createGlobalReplicationGroup_primaryReplicationGroupId = Lens.lens (\CreateGlobalReplicationGroup' {primaryReplicationGroupId} -> primaryReplicationGroupId) (\s@CreateGlobalReplicationGroup' {} a -> s {primaryReplicationGroupId = a} :: CreateGlobalReplicationGroup)

instance Core.AWSRequest CreateGlobalReplicationGroup where
  type
    AWSResponse CreateGlobalReplicationGroup =
      CreateGlobalReplicationGroupResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CreateGlobalReplicationGroupResult"
      ( \s h x ->
          CreateGlobalReplicationGroupResponse'
            Prelude.<$> (x Core..@? "GlobalReplicationGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateGlobalReplicationGroup
  where
  hashWithSalt _salt CreateGlobalReplicationGroup' {..} =
    _salt
      `Prelude.hashWithSalt` globalReplicationGroupDescription
      `Prelude.hashWithSalt` globalReplicationGroupIdSuffix
      `Prelude.hashWithSalt` primaryReplicationGroupId

instance Prelude.NFData CreateGlobalReplicationGroup where
  rnf CreateGlobalReplicationGroup' {..} =
    Prelude.rnf globalReplicationGroupDescription
      `Prelude.seq` Prelude.rnf globalReplicationGroupIdSuffix
      `Prelude.seq` Prelude.rnf primaryReplicationGroupId

instance Core.ToHeaders CreateGlobalReplicationGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateGlobalReplicationGroup where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateGlobalReplicationGroup where
  toQuery CreateGlobalReplicationGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "CreateGlobalReplicationGroup" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2015-02-02" :: Prelude.ByteString),
        "GlobalReplicationGroupDescription"
          Core.=: globalReplicationGroupDescription,
        "GlobalReplicationGroupIdSuffix"
          Core.=: globalReplicationGroupIdSuffix,
        "PrimaryReplicationGroupId"
          Core.=: primaryReplicationGroupId
      ]

-- | /See:/ 'newCreateGlobalReplicationGroupResponse' smart constructor.
data CreateGlobalReplicationGroupResponse = CreateGlobalReplicationGroupResponse'
  { globalReplicationGroup :: Prelude.Maybe GlobalReplicationGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateGlobalReplicationGroupResponse
newCreateGlobalReplicationGroupResponse pHttpStatus_ =
  CreateGlobalReplicationGroupResponse'
    { globalReplicationGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createGlobalReplicationGroupResponse_globalReplicationGroup :: Lens.Lens' CreateGlobalReplicationGroupResponse (Prelude.Maybe GlobalReplicationGroup)
createGlobalReplicationGroupResponse_globalReplicationGroup = Lens.lens (\CreateGlobalReplicationGroupResponse' {globalReplicationGroup} -> globalReplicationGroup) (\s@CreateGlobalReplicationGroupResponse' {} a -> s {globalReplicationGroup = a} :: CreateGlobalReplicationGroupResponse)

-- | The response's http status code.
createGlobalReplicationGroupResponse_httpStatus :: Lens.Lens' CreateGlobalReplicationGroupResponse Prelude.Int
createGlobalReplicationGroupResponse_httpStatus = Lens.lens (\CreateGlobalReplicationGroupResponse' {httpStatus} -> httpStatus) (\s@CreateGlobalReplicationGroupResponse' {} a -> s {httpStatus = a} :: CreateGlobalReplicationGroupResponse)

instance
  Prelude.NFData
    CreateGlobalReplicationGroupResponse
  where
  rnf CreateGlobalReplicationGroupResponse' {..} =
    Prelude.rnf globalReplicationGroup
      `Prelude.seq` Prelude.rnf httpStatus
