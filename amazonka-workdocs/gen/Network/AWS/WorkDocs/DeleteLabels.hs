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
-- Module      : Network.AWS.WorkDocs.DeleteLabels
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified list of labels from a resource.
module Network.AWS.WorkDocs.DeleteLabels
  ( -- * Creating a Request
    DeleteLabels (..),
    newDeleteLabels,

    -- * Request Lenses
    deleteLabels_labels,
    deleteLabels_authenticationToken,
    deleteLabels_deleteAll,
    deleteLabels_resourceId,

    -- * Destructuring the Response
    DeleteLabelsResponse (..),
    newDeleteLabelsResponse,

    -- * Response Lenses
    deleteLabelsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkDocs.Types

-- | /See:/ 'newDeleteLabels' smart constructor.
data DeleteLabels = DeleteLabels'
  { -- | List of labels to delete from the resource.
    labels :: Core.Maybe [Core.Text],
    -- | Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Core.Maybe (Core.Sensitive Core.Text),
    -- | Flag to request removal of all labels from the specified resource.
    deleteAll :: Core.Maybe Core.Bool,
    -- | The ID of the resource.
    resourceId :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteLabels' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'labels', 'deleteLabels_labels' - List of labels to delete from the resource.
--
-- 'authenticationToken', 'deleteLabels_authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
--
-- 'deleteAll', 'deleteLabels_deleteAll' - Flag to request removal of all labels from the specified resource.
--
-- 'resourceId', 'deleteLabels_resourceId' - The ID of the resource.
newDeleteLabels ::
  -- | 'resourceId'
  Core.Text ->
  DeleteLabels
newDeleteLabels pResourceId_ =
  DeleteLabels'
    { labels = Core.Nothing,
      authenticationToken = Core.Nothing,
      deleteAll = Core.Nothing,
      resourceId = pResourceId_
    }

-- | List of labels to delete from the resource.
deleteLabels_labels :: Lens.Lens' DeleteLabels (Core.Maybe [Core.Text])
deleteLabels_labels = Lens.lens (\DeleteLabels' {labels} -> labels) (\s@DeleteLabels' {} a -> s {labels = a} :: DeleteLabels) Core.. Lens.mapping Lens._Coerce

-- | Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
deleteLabels_authenticationToken :: Lens.Lens' DeleteLabels (Core.Maybe Core.Text)
deleteLabels_authenticationToken = Lens.lens (\DeleteLabels' {authenticationToken} -> authenticationToken) (\s@DeleteLabels' {} a -> s {authenticationToken = a} :: DeleteLabels) Core.. Lens.mapping Core._Sensitive

-- | Flag to request removal of all labels from the specified resource.
deleteLabels_deleteAll :: Lens.Lens' DeleteLabels (Core.Maybe Core.Bool)
deleteLabels_deleteAll = Lens.lens (\DeleteLabels' {deleteAll} -> deleteAll) (\s@DeleteLabels' {} a -> s {deleteAll = a} :: DeleteLabels)

-- | The ID of the resource.
deleteLabels_resourceId :: Lens.Lens' DeleteLabels Core.Text
deleteLabels_resourceId = Lens.lens (\DeleteLabels' {resourceId} -> resourceId) (\s@DeleteLabels' {} a -> s {resourceId = a} :: DeleteLabels)

instance Core.AWSRequest DeleteLabels where
  type AWSResponse DeleteLabels = DeleteLabelsResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteLabelsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteLabels

instance Core.NFData DeleteLabels

instance Core.ToHeaders DeleteLabels where
  toHeaders DeleteLabels' {..} =
    Core.mconcat
      [ "Authentication" Core.=# authenticationToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
      ]

instance Core.ToPath DeleteLabels where
  toPath DeleteLabels' {..} =
    Core.mconcat
      [ "/api/v1/resources/",
        Core.toBS resourceId,
        "/labels"
      ]

instance Core.ToQuery DeleteLabels where
  toQuery DeleteLabels' {..} =
    Core.mconcat
      [ "labels"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> labels),
        "deleteAll" Core.=: deleteAll
      ]

-- | /See:/ 'newDeleteLabelsResponse' smart constructor.
data DeleteLabelsResponse = DeleteLabelsResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteLabelsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteLabelsResponse_httpStatus' - The response's http status code.
newDeleteLabelsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteLabelsResponse
newDeleteLabelsResponse pHttpStatus_ =
  DeleteLabelsResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteLabelsResponse_httpStatus :: Lens.Lens' DeleteLabelsResponse Core.Int
deleteLabelsResponse_httpStatus = Lens.lens (\DeleteLabelsResponse' {httpStatus} -> httpStatus) (\s@DeleteLabelsResponse' {} a -> s {httpStatus = a} :: DeleteLabelsResponse)

instance Core.NFData DeleteLabelsResponse
