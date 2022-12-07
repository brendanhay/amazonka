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
-- Module      : Amazonka.ElastiCache.ModifyCacheSubnetGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies an existing cache subnet group.
module Amazonka.ElastiCache.ModifyCacheSubnetGroup
  ( -- * Creating a Request
    ModifyCacheSubnetGroup (..),
    newModifyCacheSubnetGroup,

    -- * Request Lenses
    modifyCacheSubnetGroup_cacheSubnetGroupDescription,
    modifyCacheSubnetGroup_subnetIds,
    modifyCacheSubnetGroup_cacheSubnetGroupName,

    -- * Destructuring the Response
    ModifyCacheSubnetGroupResponse (..),
    newModifyCacheSubnetGroupResponse,

    -- * Response Lenses
    modifyCacheSubnetGroupResponse_cacheSubnetGroup,
    modifyCacheSubnetGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElastiCache.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @ModifyCacheSubnetGroup@ operation.
--
-- /See:/ 'newModifyCacheSubnetGroup' smart constructor.
data ModifyCacheSubnetGroup = ModifyCacheSubnetGroup'
  { -- | A description of the cache subnet group.
    cacheSubnetGroupDescription :: Prelude.Maybe Prelude.Text,
    -- | The EC2 subnet IDs for the cache subnet group.
    subnetIds :: Prelude.Maybe [Prelude.Text],
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
-- 'cacheSubnetGroupDescription', 'modifyCacheSubnetGroup_cacheSubnetGroupDescription' - A description of the cache subnet group.
--
-- 'subnetIds', 'modifyCacheSubnetGroup_subnetIds' - The EC2 subnet IDs for the cache subnet group.
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
    { cacheSubnetGroupDescription =
        Prelude.Nothing,
      subnetIds = Prelude.Nothing,
      cacheSubnetGroupName = pCacheSubnetGroupName_
    }

-- | A description of the cache subnet group.
modifyCacheSubnetGroup_cacheSubnetGroupDescription :: Lens.Lens' ModifyCacheSubnetGroup (Prelude.Maybe Prelude.Text)
modifyCacheSubnetGroup_cacheSubnetGroupDescription = Lens.lens (\ModifyCacheSubnetGroup' {cacheSubnetGroupDescription} -> cacheSubnetGroupDescription) (\s@ModifyCacheSubnetGroup' {} a -> s {cacheSubnetGroupDescription = a} :: ModifyCacheSubnetGroup)

-- | The EC2 subnet IDs for the cache subnet group.
modifyCacheSubnetGroup_subnetIds :: Lens.Lens' ModifyCacheSubnetGroup (Prelude.Maybe [Prelude.Text])
modifyCacheSubnetGroup_subnetIds = Lens.lens (\ModifyCacheSubnetGroup' {subnetIds} -> subnetIds) (\s@ModifyCacheSubnetGroup' {} a -> s {subnetIds = a} :: ModifyCacheSubnetGroup) Prelude.. Lens.mapping Lens.coerced

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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ModifyCacheSubnetGroupResult"
      ( \s h x ->
          ModifyCacheSubnetGroupResponse'
            Prelude.<$> (x Data..@? "CacheSubnetGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyCacheSubnetGroup where
  hashWithSalt _salt ModifyCacheSubnetGroup' {..} =
    _salt
      `Prelude.hashWithSalt` cacheSubnetGroupDescription
      `Prelude.hashWithSalt` subnetIds
      `Prelude.hashWithSalt` cacheSubnetGroupName

instance Prelude.NFData ModifyCacheSubnetGroup where
  rnf ModifyCacheSubnetGroup' {..} =
    Prelude.rnf cacheSubnetGroupDescription
      `Prelude.seq` Prelude.rnf subnetIds
      `Prelude.seq` Prelude.rnf cacheSubnetGroupName

instance Data.ToHeaders ModifyCacheSubnetGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyCacheSubnetGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyCacheSubnetGroup where
  toQuery ModifyCacheSubnetGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ModifyCacheSubnetGroup" :: Prelude.ByteString),
        "Version"
          Data.=: ("2015-02-02" :: Prelude.ByteString),
        "CacheSubnetGroupDescription"
          Data.=: cacheSubnetGroupDescription,
        "SubnetIds"
          Data.=: Data.toQuery
            ( Data.toQueryList "SubnetIdentifier"
                Prelude.<$> subnetIds
            ),
        "CacheSubnetGroupName" Data.=: cacheSubnetGroupName
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
  where
  rnf ModifyCacheSubnetGroupResponse' {..} =
    Prelude.rnf cacheSubnetGroup
      `Prelude.seq` Prelude.rnf httpStatus
