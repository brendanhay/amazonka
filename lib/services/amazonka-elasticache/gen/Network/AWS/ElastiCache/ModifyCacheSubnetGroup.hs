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
-- Module      : Network.AWS.ElastiCache.ModifyCacheSubnetGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies an existing cache subnet group.
module Network.AWS.ElastiCache.ModifyCacheSubnetGroup
  ( -- * Creating a Request
    ModifyCacheSubnetGroup (..),
    newModifyCacheSubnetGroup,

    -- * Request Lenses
    modifyCacheSubnetGroup_subnetIds,
    modifyCacheSubnetGroup_cacheSubnetGroupDescription,
    modifyCacheSubnetGroup_cacheSubnetGroupName,

    -- * Destructuring the Response
    ModifyCacheSubnetGroupResponse (..),
    newModifyCacheSubnetGroupResponse,

    -- * Response Lenses
    modifyCacheSubnetGroupResponse_cacheSubnetGroup,
    modifyCacheSubnetGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @ModifyCacheSubnetGroup@ operation.
--
-- /See:/ 'newModifyCacheSubnetGroup' smart constructor.
data ModifyCacheSubnetGroup = ModifyCacheSubnetGroup'
  { -- | The EC2 subnet IDs for the cache subnet group.
    subnetIds :: Prelude.Maybe [Prelude.Text],
    -- | A description of the cache subnet group.
    cacheSubnetGroupDescription :: Prelude.Maybe Prelude.Text,
    -- | The name for the cache subnet group. This value is stored as a lowercase
    -- string.
    --
    -- Constraints: Must contain no more than 255 alphanumeric characters or
    -- hyphens.
    --
    -- Example: @mysubnetgroup@
    cacheSubnetGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyCacheSubnetGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subnetIds', 'modifyCacheSubnetGroup_subnetIds' - The EC2 subnet IDs for the cache subnet group.
--
-- 'cacheSubnetGroupDescription', 'modifyCacheSubnetGroup_cacheSubnetGroupDescription' - A description of the cache subnet group.
--
-- 'cacheSubnetGroupName', 'modifyCacheSubnetGroup_cacheSubnetGroupName' - The name for the cache subnet group. This value is stored as a lowercase
-- string.
--
-- Constraints: Must contain no more than 255 alphanumeric characters or
-- hyphens.
--
-- Example: @mysubnetgroup@
newModifyCacheSubnetGroup ::
  -- | 'cacheSubnetGroupName'
  Prelude.Text ->
  ModifyCacheSubnetGroup
newModifyCacheSubnetGroup pCacheSubnetGroupName_ =
  ModifyCacheSubnetGroup'
    { subnetIds =
        Prelude.Nothing,
      cacheSubnetGroupDescription = Prelude.Nothing,
      cacheSubnetGroupName = pCacheSubnetGroupName_
    }

-- | The EC2 subnet IDs for the cache subnet group.
modifyCacheSubnetGroup_subnetIds :: Lens.Lens' ModifyCacheSubnetGroup (Prelude.Maybe [Prelude.Text])
modifyCacheSubnetGroup_subnetIds = Lens.lens (\ModifyCacheSubnetGroup' {subnetIds} -> subnetIds) (\s@ModifyCacheSubnetGroup' {} a -> s {subnetIds = a} :: ModifyCacheSubnetGroup) Prelude.. Lens.mapping Lens.coerced

-- | A description of the cache subnet group.
modifyCacheSubnetGroup_cacheSubnetGroupDescription :: Lens.Lens' ModifyCacheSubnetGroup (Prelude.Maybe Prelude.Text)
modifyCacheSubnetGroup_cacheSubnetGroupDescription = Lens.lens (\ModifyCacheSubnetGroup' {cacheSubnetGroupDescription} -> cacheSubnetGroupDescription) (\s@ModifyCacheSubnetGroup' {} a -> s {cacheSubnetGroupDescription = a} :: ModifyCacheSubnetGroup)

-- | The name for the cache subnet group. This value is stored as a lowercase
-- string.
--
-- Constraints: Must contain no more than 255 alphanumeric characters or
-- hyphens.
--
-- Example: @mysubnetgroup@
modifyCacheSubnetGroup_cacheSubnetGroupName :: Lens.Lens' ModifyCacheSubnetGroup Prelude.Text
modifyCacheSubnetGroup_cacheSubnetGroupName = Lens.lens (\ModifyCacheSubnetGroup' {cacheSubnetGroupName} -> cacheSubnetGroupName) (\s@ModifyCacheSubnetGroup' {} a -> s {cacheSubnetGroupName = a} :: ModifyCacheSubnetGroup)

instance Core.AWSRequest ModifyCacheSubnetGroup where
  type
    AWSResponse ModifyCacheSubnetGroup =
      ModifyCacheSubnetGroupResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ModifyCacheSubnetGroupResult"
      ( \s h x ->
          ModifyCacheSubnetGroupResponse'
            Prelude.<$> (x Core..@? "CacheSubnetGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyCacheSubnetGroup

instance Prelude.NFData ModifyCacheSubnetGroup

instance Core.ToHeaders ModifyCacheSubnetGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ModifyCacheSubnetGroup where
  toPath = Prelude.const "/"

instance Core.ToQuery ModifyCacheSubnetGroup where
  toQuery ModifyCacheSubnetGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ModifyCacheSubnetGroup" :: Prelude.ByteString),
        "Version"
          Core.=: ("2015-02-02" :: Prelude.ByteString),
        "SubnetIds"
          Core.=: Core.toQuery
            ( Core.toQueryList "SubnetIdentifier"
                Prelude.<$> subnetIds
            ),
        "CacheSubnetGroupDescription"
          Core.=: cacheSubnetGroupDescription,
        "CacheSubnetGroupName" Core.=: cacheSubnetGroupName
      ]

-- | /See:/ 'newModifyCacheSubnetGroupResponse' smart constructor.
data ModifyCacheSubnetGroupResponse = ModifyCacheSubnetGroupResponse'
  { cacheSubnetGroup :: Prelude.Maybe CacheSubnetGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyCacheSubnetGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cacheSubnetGroup', 'modifyCacheSubnetGroupResponse_cacheSubnetGroup' - Undocumented member.
--
-- 'httpStatus', 'modifyCacheSubnetGroupResponse_httpStatus' - The response's http status code.
newModifyCacheSubnetGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyCacheSubnetGroupResponse
newModifyCacheSubnetGroupResponse pHttpStatus_ =
  ModifyCacheSubnetGroupResponse'
    { cacheSubnetGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
modifyCacheSubnetGroupResponse_cacheSubnetGroup :: Lens.Lens' ModifyCacheSubnetGroupResponse (Prelude.Maybe CacheSubnetGroup)
modifyCacheSubnetGroupResponse_cacheSubnetGroup = Lens.lens (\ModifyCacheSubnetGroupResponse' {cacheSubnetGroup} -> cacheSubnetGroup) (\s@ModifyCacheSubnetGroupResponse' {} a -> s {cacheSubnetGroup = a} :: ModifyCacheSubnetGroupResponse)

-- | The response's http status code.
modifyCacheSubnetGroupResponse_httpStatus :: Lens.Lens' ModifyCacheSubnetGroupResponse Prelude.Int
modifyCacheSubnetGroupResponse_httpStatus = Lens.lens (\ModifyCacheSubnetGroupResponse' {httpStatus} -> httpStatus) (\s@ModifyCacheSubnetGroupResponse' {} a -> s {httpStatus = a} :: ModifyCacheSubnetGroupResponse)

instance
  Prelude.NFData
    ModifyCacheSubnetGroupResponse
