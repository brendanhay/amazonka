{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ElastiCache.DeleteCacheParameterGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified cache parameter group. You cannot delete a cache
-- parameter group if it is associated with any cache clusters. You cannot
-- delete the default cache parameter groups in your account.
module Network.AWS.ElastiCache.DeleteCacheParameterGroup
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

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DeleteCacheParameterGroup where
  type
    Rs DeleteCacheParameterGroup =
      DeleteCacheParameterGroupResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      DeleteCacheParameterGroupResponse'

instance Prelude.Hashable DeleteCacheParameterGroup

instance Prelude.NFData DeleteCacheParameterGroup

instance Prelude.ToHeaders DeleteCacheParameterGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteCacheParameterGroup where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteCacheParameterGroup where
  toQuery DeleteCacheParameterGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DeleteCacheParameterGroup" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2015-02-02" :: Prelude.ByteString),
        "CacheParameterGroupName"
          Prelude.=: cacheParameterGroupName
      ]

-- | /See:/ 'newDeleteCacheParameterGroupResponse' smart constructor.
data DeleteCacheParameterGroupResponse = DeleteCacheParameterGroupResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
