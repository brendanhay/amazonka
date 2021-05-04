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
-- Module      : Network.AWS.EFS.DeleteAccessPoint
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.EFS.DeleteAccessPoint
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

import Network.AWS.EFS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteAccessPoint' smart constructor.
data DeleteAccessPoint = DeleteAccessPoint'
  { -- | The ID of the access point that you want to delete.
    accessPointId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DeleteAccessPoint where
  type Rs DeleteAccessPoint = DeleteAccessPointResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull DeleteAccessPointResponse'

instance Prelude.Hashable DeleteAccessPoint

instance Prelude.NFData DeleteAccessPoint

instance Prelude.ToHeaders DeleteAccessPoint where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteAccessPoint where
  toPath DeleteAccessPoint' {..} =
    Prelude.mconcat
      [ "/2015-02-01/access-points/",
        Prelude.toBS accessPointId
      ]

instance Prelude.ToQuery DeleteAccessPoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAccessPointResponse' smart constructor.
data DeleteAccessPointResponse = DeleteAccessPointResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteAccessPointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteAccessPointResponse ::
  DeleteAccessPointResponse
newDeleteAccessPointResponse =
  DeleteAccessPointResponse'

instance Prelude.NFData DeleteAccessPointResponse
