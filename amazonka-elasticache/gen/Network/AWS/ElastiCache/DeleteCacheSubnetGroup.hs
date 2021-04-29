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
-- Module      : Network.AWS.ElastiCache.DeleteCacheSubnetGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a cache subnet group.
--
-- You cannot delete a default cache subnet group or one that is associated
-- with any clusters.
module Network.AWS.ElastiCache.DeleteCacheSubnetGroup
  ( -- * Creating a Request
    DeleteCacheSubnetGroup (..),
    newDeleteCacheSubnetGroup,

    -- * Request Lenses
    deleteCacheSubnetGroup_cacheSubnetGroupName,

    -- * Destructuring the Response
    DeleteCacheSubnetGroupResponse (..),
    newDeleteCacheSubnetGroupResponse,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DeleteCacheSubnetGroup@ operation.
--
-- /See:/ 'newDeleteCacheSubnetGroup' smart constructor.
data DeleteCacheSubnetGroup = DeleteCacheSubnetGroup'
  { -- | The name of the cache subnet group to delete.
    --
    -- Constraints: Must contain no more than 255 alphanumeric characters or
    -- hyphens.
    cacheSubnetGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteCacheSubnetGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cacheSubnetGroupName', 'deleteCacheSubnetGroup_cacheSubnetGroupName' - The name of the cache subnet group to delete.
--
-- Constraints: Must contain no more than 255 alphanumeric characters or
-- hyphens.
newDeleteCacheSubnetGroup ::
  -- | 'cacheSubnetGroupName'
  Prelude.Text ->
  DeleteCacheSubnetGroup
newDeleteCacheSubnetGroup pCacheSubnetGroupName_ =
  DeleteCacheSubnetGroup'
    { cacheSubnetGroupName =
        pCacheSubnetGroupName_
    }

-- | The name of the cache subnet group to delete.
--
-- Constraints: Must contain no more than 255 alphanumeric characters or
-- hyphens.
deleteCacheSubnetGroup_cacheSubnetGroupName :: Lens.Lens' DeleteCacheSubnetGroup Prelude.Text
deleteCacheSubnetGroup_cacheSubnetGroupName = Lens.lens (\DeleteCacheSubnetGroup' {cacheSubnetGroupName} -> cacheSubnetGroupName) (\s@DeleteCacheSubnetGroup' {} a -> s {cacheSubnetGroupName = a} :: DeleteCacheSubnetGroup)

instance Prelude.AWSRequest DeleteCacheSubnetGroup where
  type
    Rs DeleteCacheSubnetGroup =
      DeleteCacheSubnetGroupResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      DeleteCacheSubnetGroupResponse'

instance Prelude.Hashable DeleteCacheSubnetGroup

instance Prelude.NFData DeleteCacheSubnetGroup

instance Prelude.ToHeaders DeleteCacheSubnetGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteCacheSubnetGroup where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteCacheSubnetGroup where
  toQuery DeleteCacheSubnetGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DeleteCacheSubnetGroup" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2015-02-02" :: Prelude.ByteString),
        "CacheSubnetGroupName"
          Prelude.=: cacheSubnetGroupName
      ]

-- | /See:/ 'newDeleteCacheSubnetGroupResponse' smart constructor.
data DeleteCacheSubnetGroupResponse = DeleteCacheSubnetGroupResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteCacheSubnetGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteCacheSubnetGroupResponse ::
  DeleteCacheSubnetGroupResponse
newDeleteCacheSubnetGroupResponse =
  DeleteCacheSubnetGroupResponse'

instance
  Prelude.NFData
    DeleteCacheSubnetGroupResponse
