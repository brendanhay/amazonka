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
-- Module      : Amazonka.Redshift.DeleteSnapshotCopyGrant
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified snapshot copy grant.
module Amazonka.Redshift.DeleteSnapshotCopyGrant
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The result of the @DeleteSnapshotCopyGrant@ action.
--
-- /See:/ 'newDeleteSnapshotCopyGrant' smart constructor.
data DeleteSnapshotCopyGrant = DeleteSnapshotCopyGrant'
  { -- | The name of the snapshot copy grant to delete.
    snapshotCopyGrantName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DeleteSnapshotCopyGrant where
  type
    AWSResponse DeleteSnapshotCopyGrant =
      DeleteSnapshotCopyGrantResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull
      DeleteSnapshotCopyGrantResponse'

instance Prelude.Hashable DeleteSnapshotCopyGrant where
  hashWithSalt _salt DeleteSnapshotCopyGrant' {..} =
    _salt `Prelude.hashWithSalt` snapshotCopyGrantName

instance Prelude.NFData DeleteSnapshotCopyGrant where
  rnf DeleteSnapshotCopyGrant' {..} =
    Prelude.rnf snapshotCopyGrantName

instance Data.ToHeaders DeleteSnapshotCopyGrant where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteSnapshotCopyGrant where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteSnapshotCopyGrant where
  toQuery DeleteSnapshotCopyGrant' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteSnapshotCopyGrant" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "SnapshotCopyGrantName"
          Data.=: snapshotCopyGrantName
      ]

-- | /See:/ 'newDeleteSnapshotCopyGrantResponse' smart constructor.
data DeleteSnapshotCopyGrantResponse = DeleteSnapshotCopyGrantResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf _ = ()
