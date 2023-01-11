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
-- Module      : Amazonka.ElastiCache.DeleteCacheSecurityGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a cache security group.
--
-- You cannot delete a cache security group if it is associated with any
-- clusters.
module Amazonka.ElastiCache.DeleteCacheSecurityGroup
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElastiCache.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @DeleteCacheSecurityGroup@ operation.
--
-- /See:/ 'newDeleteCacheSecurityGroup' smart constructor.
data DeleteCacheSecurityGroup = DeleteCacheSecurityGroup'
  { -- | The name of the cache security group to delete.
    --
    -- You cannot delete the default security group.
    cacheSecurityGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DeleteCacheSecurityGroup
newDeleteCacheSecurityGroup pCacheSecurityGroupName_ =
  DeleteCacheSecurityGroup'
    { cacheSecurityGroupName =
        pCacheSecurityGroupName_
    }

-- | The name of the cache security group to delete.
--
-- You cannot delete the default security group.
deleteCacheSecurityGroup_cacheSecurityGroupName :: Lens.Lens' DeleteCacheSecurityGroup Prelude.Text
deleteCacheSecurityGroup_cacheSecurityGroupName = Lens.lens (\DeleteCacheSecurityGroup' {cacheSecurityGroupName} -> cacheSecurityGroupName) (\s@DeleteCacheSecurityGroup' {} a -> s {cacheSecurityGroupName = a} :: DeleteCacheSecurityGroup)

instance Core.AWSRequest DeleteCacheSecurityGroup where
  type
    AWSResponse DeleteCacheSecurityGroup =
      DeleteCacheSecurityGroupResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull
      DeleteCacheSecurityGroupResponse'

instance Prelude.Hashable DeleteCacheSecurityGroup where
  hashWithSalt _salt DeleteCacheSecurityGroup' {..} =
    _salt `Prelude.hashWithSalt` cacheSecurityGroupName

instance Prelude.NFData DeleteCacheSecurityGroup where
  rnf DeleteCacheSecurityGroup' {..} =
    Prelude.rnf cacheSecurityGroupName

instance Data.ToHeaders DeleteCacheSecurityGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteCacheSecurityGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteCacheSecurityGroup where
  toQuery DeleteCacheSecurityGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteCacheSecurityGroup" :: Prelude.ByteString),
        "Version"
          Data.=: ("2015-02-02" :: Prelude.ByteString),
        "CacheSecurityGroupName"
          Data.=: cacheSecurityGroupName
      ]

-- | /See:/ 'newDeleteCacheSecurityGroupResponse' smart constructor.
data DeleteCacheSecurityGroupResponse = DeleteCacheSecurityGroupResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCacheSecurityGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteCacheSecurityGroupResponse ::
  DeleteCacheSecurityGroupResponse
newDeleteCacheSecurityGroupResponse =
  DeleteCacheSecurityGroupResponse'

instance
  Prelude.NFData
    DeleteCacheSecurityGroupResponse
  where
  rnf _ = ()
