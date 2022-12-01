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
-- Module      : Amazonka.ElastiCache.DeleteCacheParameterGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified cache parameter group. You cannot delete a cache
-- parameter group if it is associated with any cache clusters. You cannot
-- delete the default cache parameter groups in your account.
module Amazonka.ElastiCache.DeleteCacheParameterGroup
  ( -- * Creating a Request
    DeleteCacheParameterGroup (..),
    newDeleteCacheParameterGroup,

    -- * Request Lenses
    deleteCacheParameterGroup_cacheParameterGroupName,

    -- * Destructuring the Response
    DeleteCacheParameterGroupResponse (..),
    newDeleteCacheParameterGroupResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ElastiCache.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @DeleteCacheParameterGroup@ operation.
--
-- /See:/ 'newDeleteCacheParameterGroup' smart constructor.
data DeleteCacheParameterGroup = DeleteCacheParameterGroup'
  { -- | The name of the cache parameter group to delete.
    --
    -- The specified cache security group must not be associated with any
    -- clusters.
    cacheParameterGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCacheParameterGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cacheParameterGroupName', 'deleteCacheParameterGroup_cacheParameterGroupName' - The name of the cache parameter group to delete.
--
-- The specified cache security group must not be associated with any
-- clusters.
newDeleteCacheParameterGroup ::
  -- | 'cacheParameterGroupName'
  Prelude.Text ->
  DeleteCacheParameterGroup
newDeleteCacheParameterGroup
  pCacheParameterGroupName_ =
    DeleteCacheParameterGroup'
      { cacheParameterGroupName =
          pCacheParameterGroupName_
      }

-- | The name of the cache parameter group to delete.
--
-- The specified cache security group must not be associated with any
-- clusters.
deleteCacheParameterGroup_cacheParameterGroupName :: Lens.Lens' DeleteCacheParameterGroup Prelude.Text
deleteCacheParameterGroup_cacheParameterGroupName = Lens.lens (\DeleteCacheParameterGroup' {cacheParameterGroupName} -> cacheParameterGroupName) (\s@DeleteCacheParameterGroup' {} a -> s {cacheParameterGroupName = a} :: DeleteCacheParameterGroup)

instance Core.AWSRequest DeleteCacheParameterGroup where
  type
    AWSResponse DeleteCacheParameterGroup =
      DeleteCacheParameterGroupResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull
      DeleteCacheParameterGroupResponse'

instance Prelude.Hashable DeleteCacheParameterGroup where
  hashWithSalt _salt DeleteCacheParameterGroup' {..} =
    _salt
      `Prelude.hashWithSalt` cacheParameterGroupName

instance Prelude.NFData DeleteCacheParameterGroup where
  rnf DeleteCacheParameterGroup' {..} =
    Prelude.rnf cacheParameterGroupName

instance Core.ToHeaders DeleteCacheParameterGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteCacheParameterGroup where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteCacheParameterGroup where
  toQuery DeleteCacheParameterGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DeleteCacheParameterGroup" :: Prelude.ByteString),
        "Version"
          Core.=: ("2015-02-02" :: Prelude.ByteString),
        "CacheParameterGroupName"
          Core.=: cacheParameterGroupName
      ]

-- | /See:/ 'newDeleteCacheParameterGroupResponse' smart constructor.
data DeleteCacheParameterGroupResponse = DeleteCacheParameterGroupResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCacheParameterGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteCacheParameterGroupResponse ::
  DeleteCacheParameterGroupResponse
newDeleteCacheParameterGroupResponse =
  DeleteCacheParameterGroupResponse'

instance
  Prelude.NFData
    DeleteCacheParameterGroupResponse
  where
  rnf _ = ()
