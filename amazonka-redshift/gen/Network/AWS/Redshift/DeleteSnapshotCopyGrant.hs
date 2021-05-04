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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The result of the @DeleteSnapshotCopyGrant@ action.
--
-- /See:/ 'newDeleteSnapshotCopyGrant' smart constructor.
data DeleteSnapshotCopyGrant = DeleteSnapshotCopyGrant'
  { -- | The name of the snapshot copy grant to delete.
    snapshotCopyGrantName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DeleteSnapshotCopyGrant
newDeleteSnapshotCopyGrant pSnapshotCopyGrantName_ =
  DeleteSnapshotCopyGrant'
    { snapshotCopyGrantName =
        pSnapshotCopyGrantName_
    }

-- | The name of the snapshot copy grant to delete.
deleteSnapshotCopyGrant_snapshotCopyGrantName :: Lens.Lens' DeleteSnapshotCopyGrant Prelude.Text
deleteSnapshotCopyGrant_snapshotCopyGrantName = Lens.lens (\DeleteSnapshotCopyGrant' {snapshotCopyGrantName} -> snapshotCopyGrantName) (\s@DeleteSnapshotCopyGrant' {} a -> s {snapshotCopyGrantName = a} :: DeleteSnapshotCopyGrant)

instance Prelude.AWSRequest DeleteSnapshotCopyGrant where
  type
    Rs DeleteSnapshotCopyGrant =
      DeleteSnapshotCopyGrantResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      DeleteSnapshotCopyGrantResponse'

instance Prelude.Hashable DeleteSnapshotCopyGrant

instance Prelude.NFData DeleteSnapshotCopyGrant

instance Prelude.ToHeaders DeleteSnapshotCopyGrant where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteSnapshotCopyGrant where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteSnapshotCopyGrant where
  toQuery DeleteSnapshotCopyGrant' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DeleteSnapshotCopyGrant" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2012-12-01" :: Prelude.ByteString),
        "SnapshotCopyGrantName"
          Prelude.=: snapshotCopyGrantName
      ]

-- | /See:/ 'newDeleteSnapshotCopyGrantResponse' smart constructor.
data DeleteSnapshotCopyGrantResponse = DeleteSnapshotCopyGrantResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteSnapshotCopyGrantResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteSnapshotCopyGrantResponse ::
  DeleteSnapshotCopyGrantResponse
newDeleteSnapshotCopyGrantResponse =
  DeleteSnapshotCopyGrantResponse'

instance
  Prelude.NFData
    DeleteSnapshotCopyGrantResponse
