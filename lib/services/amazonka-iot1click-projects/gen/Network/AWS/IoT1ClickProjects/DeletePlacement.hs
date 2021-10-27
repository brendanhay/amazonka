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
-- Module      : Network.AWS.IoT1ClickProjects.DeletePlacement
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a placement. To delete a placement, it must not have any devices
-- associated with it.
--
-- When you delete a placement, all associated data becomes irretrievable.
module Network.AWS.IoT1ClickProjects.DeletePlacement
  ( -- * Creating a Request
    DeletePlacement (..),
    newDeletePlacement,

    -- * Request Lenses
    deletePlacement_placementName,
    deletePlacement_projectName,

    -- * Destructuring the Response
    DeletePlacementResponse (..),
    newDeletePlacementResponse,

    -- * Response Lenses
    deletePlacementResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT1ClickProjects.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeletePlacement' smart constructor.
data DeletePlacement = DeletePlacement'
  { -- | The name of the empty placement to delete.
    placementName :: Prelude.Text,
    -- | The project containing the empty placement to delete.
    projectName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePlacement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'placementName', 'deletePlacement_placementName' - The name of the empty placement to delete.
--
-- 'projectName', 'deletePlacement_projectName' - The project containing the empty placement to delete.
newDeletePlacement ::
  -- | 'placementName'
  Prelude.Text ->
  -- | 'projectName'
  Prelude.Text ->
  DeletePlacement
newDeletePlacement pPlacementName_ pProjectName_ =
  DeletePlacement'
    { placementName = pPlacementName_,
      projectName = pProjectName_
    }

-- | The name of the empty placement to delete.
deletePlacement_placementName :: Lens.Lens' DeletePlacement Prelude.Text
deletePlacement_placementName = Lens.lens (\DeletePlacement' {placementName} -> placementName) (\s@DeletePlacement' {} a -> s {placementName = a} :: DeletePlacement)

-- | The project containing the empty placement to delete.
deletePlacement_projectName :: Lens.Lens' DeletePlacement Prelude.Text
deletePlacement_projectName = Lens.lens (\DeletePlacement' {projectName} -> projectName) (\s@DeletePlacement' {} a -> s {projectName = a} :: DeletePlacement)

instance Core.AWSRequest DeletePlacement where
  type
    AWSResponse DeletePlacement =
      DeletePlacementResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeletePlacementResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeletePlacement

instance Prelude.NFData DeletePlacement

instance Core.ToHeaders DeletePlacement where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeletePlacement where
  toPath DeletePlacement' {..} =
    Prelude.mconcat
      [ "/projects/",
        Core.toBS projectName,
        "/placements/",
        Core.toBS placementName
      ]

instance Core.ToQuery DeletePlacement where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeletePlacementResponse' smart constructor.
data DeletePlacementResponse = DeletePlacementResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePlacementResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deletePlacementResponse_httpStatus' - The response's http status code.
newDeletePlacementResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeletePlacementResponse
newDeletePlacementResponse pHttpStatus_ =
  DeletePlacementResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deletePlacementResponse_httpStatus :: Lens.Lens' DeletePlacementResponse Prelude.Int
deletePlacementResponse_httpStatus = Lens.lens (\DeletePlacementResponse' {httpStatus} -> httpStatus) (\s@DeletePlacementResponse' {} a -> s {httpStatus = a} :: DeletePlacementResponse)

instance Prelude.NFData DeletePlacementResponse
