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
-- Module      : Network.AWS.Redshift.DeleteSnapshotCopyGrant
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified snapshot copy grant.
module Network.AWS.Redshift.DeleteSnapshotCopyGrant
  ( -- * Creating a Request
    DeleteSnapshotCopyGrant (..),
    newDeleteSnapshotCopyGrant,

    -- * Request Lenses
    deleteSnapshotCopyGrant_snapshotCopyGrantName,

    -- * Destructuring the Response
    DeleteSnapshotCopyGrantResponse (..),
    newDeleteSnapshotCopyGrantResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The result of the @DeleteSnapshotCopyGrant@ action.
--
-- /See:/ 'newDeleteSnapshotCopyGrant' smart constructor.
data DeleteSnapshotCopyGrant = DeleteSnapshotCopyGrant'
  { -- | The name of the snapshot copy grant to delete.
    snapshotCopyGrantName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteSnapshotCopyGrant' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snapshotCopyGrantName', 'deleteSnapshotCopyGrant_snapshotCopyGrantName' - The name of the snapshot copy grant to delete.
newDeleteSnapshotCopyGrant ::
  -- | 'snapshotCopyGrantName'
  Core.Text ->
  DeleteSnapshotCopyGrant
newDeleteSnapshotCopyGrant pSnapshotCopyGrantName_ =
  DeleteSnapshotCopyGrant'
    { snapshotCopyGrantName =
        pSnapshotCopyGrantName_
    }

-- | The name of the snapshot copy grant to delete.
deleteSnapshotCopyGrant_snapshotCopyGrantName :: Lens.Lens' DeleteSnapshotCopyGrant Core.Text
deleteSnapshotCopyGrant_snapshotCopyGrantName = Lens.lens (\DeleteSnapshotCopyGrant' {snapshotCopyGrantName} -> snapshotCopyGrantName) (\s@DeleteSnapshotCopyGrant' {} a -> s {snapshotCopyGrantName = a} :: DeleteSnapshotCopyGrant)

instance Core.AWSRequest DeleteSnapshotCopyGrant where
  type
    AWSResponse DeleteSnapshotCopyGrant =
      DeleteSnapshotCopyGrantResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      DeleteSnapshotCopyGrantResponse'

instance Core.Hashable DeleteSnapshotCopyGrant

instance Core.NFData DeleteSnapshotCopyGrant

instance Core.ToHeaders DeleteSnapshotCopyGrant where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteSnapshotCopyGrant where
  toPath = Core.const "/"

instance Core.ToQuery DeleteSnapshotCopyGrant where
  toQuery DeleteSnapshotCopyGrant' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DeleteSnapshotCopyGrant" :: Core.ByteString),
        "Version" Core.=: ("2012-12-01" :: Core.ByteString),
        "SnapshotCopyGrantName"
          Core.=: snapshotCopyGrantName
      ]

-- | /See:/ 'newDeleteSnapshotCopyGrantResponse' smart constructor.
data DeleteSnapshotCopyGrantResponse = DeleteSnapshotCopyGrantResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteSnapshotCopyGrantResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteSnapshotCopyGrantResponse ::
  DeleteSnapshotCopyGrantResponse
newDeleteSnapshotCopyGrantResponse =
  DeleteSnapshotCopyGrantResponse'

instance Core.NFData DeleteSnapshotCopyGrantResponse
