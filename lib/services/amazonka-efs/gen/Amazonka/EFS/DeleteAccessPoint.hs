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
-- Module      : Amazonka.EFS.DeleteAccessPoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified access point. After deletion is complete, new
-- clients can no longer connect to the access points. Clients connected to
-- the access point at the time of deletion will continue to function until
-- they terminate their connection.
--
-- This operation requires permissions for the
-- @elasticfilesystem:DeleteAccessPoint@ action.
module Amazonka.EFS.DeleteAccessPoint
  ( -- * Creating a Request
    DeleteAccessPoint (..),
    newDeleteAccessPoint,

    -- * Request Lenses
    deleteAccessPoint_accessPointId,

    -- * Destructuring the Response
    DeleteAccessPointResponse (..),
    newDeleteAccessPointResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EFS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteAccessPoint' smart constructor.
data DeleteAccessPoint = DeleteAccessPoint'
  { -- | The ID of the access point that you want to delete.
    accessPointId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAccessPoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessPointId', 'deleteAccessPoint_accessPointId' - The ID of the access point that you want to delete.
newDeleteAccessPoint ::
  -- | 'accessPointId'
  Prelude.Text ->
  DeleteAccessPoint
newDeleteAccessPoint pAccessPointId_ =
  DeleteAccessPoint' {accessPointId = pAccessPointId_}

-- | The ID of the access point that you want to delete.
deleteAccessPoint_accessPointId :: Lens.Lens' DeleteAccessPoint Prelude.Text
deleteAccessPoint_accessPointId = Lens.lens (\DeleteAccessPoint' {accessPointId} -> accessPointId) (\s@DeleteAccessPoint' {} a -> s {accessPointId = a} :: DeleteAccessPoint)

instance Core.AWSRequest DeleteAccessPoint where
  type
    AWSResponse DeleteAccessPoint =
      DeleteAccessPointResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteAccessPointResponse'

instance Prelude.Hashable DeleteAccessPoint where
  hashWithSalt _salt DeleteAccessPoint' {..} =
    _salt `Prelude.hashWithSalt` accessPointId

instance Prelude.NFData DeleteAccessPoint where
  rnf DeleteAccessPoint' {..} =
    Prelude.rnf accessPointId

instance Data.ToHeaders DeleteAccessPoint where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteAccessPoint where
  toPath DeleteAccessPoint' {..} =
    Prelude.mconcat
      [ "/2015-02-01/access-points/",
        Data.toBS accessPointId
      ]

instance Data.ToQuery DeleteAccessPoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAccessPointResponse' smart constructor.
data DeleteAccessPointResponse = DeleteAccessPointResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAccessPointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteAccessPointResponse ::
  DeleteAccessPointResponse
newDeleteAccessPointResponse =
  DeleteAccessPointResponse'

instance Prelude.NFData DeleteAccessPointResponse where
  rnf _ = ()
