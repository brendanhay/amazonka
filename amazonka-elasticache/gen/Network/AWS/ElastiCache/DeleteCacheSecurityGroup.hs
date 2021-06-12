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
-- Module      : Network.AWS.ElastiCache.DeleteCacheSecurityGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a cache security group.
--
-- You cannot delete a cache security group if it is associated with any
-- clusters.
module Network.AWS.ElastiCache.DeleteCacheSecurityGroup
  ( -- * Creating a Request
    DeleteCacheSecurityGroup (..),
    newDeleteCacheSecurityGroup,

    -- * Request Lenses
    deleteCacheSecurityGroup_cacheSecurityGroupName,

    -- * Destructuring the Response
    DeleteCacheSecurityGroupResponse (..),
    newDeleteCacheSecurityGroupResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DeleteCacheSecurityGroup@ operation.
--
-- /See:/ 'newDeleteCacheSecurityGroup' smart constructor.
data DeleteCacheSecurityGroup = DeleteCacheSecurityGroup'
  { -- | The name of the cache security group to delete.
    --
    -- You cannot delete the default security group.
    cacheSecurityGroupName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteCacheSecurityGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cacheSecurityGroupName', 'deleteCacheSecurityGroup_cacheSecurityGroupName' - The name of the cache security group to delete.
--
-- You cannot delete the default security group.
newDeleteCacheSecurityGroup ::
  -- | 'cacheSecurityGroupName'
  Core.Text ->
  DeleteCacheSecurityGroup
newDeleteCacheSecurityGroup pCacheSecurityGroupName_ =
  DeleteCacheSecurityGroup'
    { cacheSecurityGroupName =
        pCacheSecurityGroupName_
    }

-- | The name of the cache security group to delete.
--
-- You cannot delete the default security group.
deleteCacheSecurityGroup_cacheSecurityGroupName :: Lens.Lens' DeleteCacheSecurityGroup Core.Text
deleteCacheSecurityGroup_cacheSecurityGroupName = Lens.lens (\DeleteCacheSecurityGroup' {cacheSecurityGroupName} -> cacheSecurityGroupName) (\s@DeleteCacheSecurityGroup' {} a -> s {cacheSecurityGroupName = a} :: DeleteCacheSecurityGroup)

instance Core.AWSRequest DeleteCacheSecurityGroup where
  type
    AWSResponse DeleteCacheSecurityGroup =
      DeleteCacheSecurityGroupResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      DeleteCacheSecurityGroupResponse'

instance Core.Hashable DeleteCacheSecurityGroup

instance Core.NFData DeleteCacheSecurityGroup

instance Core.ToHeaders DeleteCacheSecurityGroup where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteCacheSecurityGroup where
  toPath = Core.const "/"

instance Core.ToQuery DeleteCacheSecurityGroup where
  toQuery DeleteCacheSecurityGroup' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DeleteCacheSecurityGroup" :: Core.ByteString),
        "Version" Core.=: ("2015-02-02" :: Core.ByteString),
        "CacheSecurityGroupName"
          Core.=: cacheSecurityGroupName
      ]

-- | /See:/ 'newDeleteCacheSecurityGroupResponse' smart constructor.
data DeleteCacheSecurityGroupResponse = DeleteCacheSecurityGroupResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteCacheSecurityGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteCacheSecurityGroupResponse ::
  DeleteCacheSecurityGroupResponse
newDeleteCacheSecurityGroupResponse =
  DeleteCacheSecurityGroupResponse'

instance Core.NFData DeleteCacheSecurityGroupResponse
