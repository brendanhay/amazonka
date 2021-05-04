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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkDocs.Types

-- | /See:/ 'newDeleteLabels' smart constructor.
data DeleteLabels = DeleteLabels'
  { -- | List of labels to delete from the resource.
    labels :: Prelude.Maybe [Prelude.Text],
    -- | Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | Flag to request removal of all labels from the specified resource.
    deleteAll :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the resource.
    resourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DeleteLabels
newDeleteLabels pResourceId_ =
  DeleteLabels'
    { labels = Prelude.Nothing,
      authenticationToken = Prelude.Nothing,
      deleteAll = Prelude.Nothing,
      resourceId = pResourceId_
    }

-- | List of labels to delete from the resource.
deleteLabels_labels :: Lens.Lens' DeleteLabels (Prelude.Maybe [Prelude.Text])
deleteLabels_labels = Lens.lens (\DeleteLabels' {labels} -> labels) (\s@DeleteLabels' {} a -> s {labels = a} :: DeleteLabels) Prelude.. Lens.mapping Prelude._Coerce

-- | Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
deleteLabels_authenticationToken :: Lens.Lens' DeleteLabels (Prelude.Maybe Prelude.Text)
deleteLabels_authenticationToken = Lens.lens (\DeleteLabels' {authenticationToken} -> authenticationToken) (\s@DeleteLabels' {} a -> s {authenticationToken = a} :: DeleteLabels) Prelude.. Lens.mapping Prelude._Sensitive

-- | Flag to request removal of all labels from the specified resource.
deleteLabels_deleteAll :: Lens.Lens' DeleteLabels (Prelude.Maybe Prelude.Bool)
deleteLabels_deleteAll = Lens.lens (\DeleteLabels' {deleteAll} -> deleteAll) (\s@DeleteLabels' {} a -> s {deleteAll = a} :: DeleteLabels)

-- | The ID of the resource.
deleteLabels_resourceId :: Lens.Lens' DeleteLabels Prelude.Text
deleteLabels_resourceId = Lens.lens (\DeleteLabels' {resourceId} -> resourceId) (\s@DeleteLabels' {} a -> s {resourceId = a} :: DeleteLabels)

instance Prelude.AWSRequest DeleteLabels where
  type Rs DeleteLabels = DeleteLabelsResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteLabelsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteLabels

instance Prelude.NFData DeleteLabels

instance Prelude.ToHeaders DeleteLabels where
  toHeaders DeleteLabels' {..} =
    Prelude.mconcat
      [ "Authentication" Prelude.=# authenticationToken,
        "Content-Type"
          Prelude.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Prelude.ToPath DeleteLabels where
  toPath DeleteLabels' {..} =
    Prelude.mconcat
      [ "/api/v1/resources/",
        Prelude.toBS resourceId,
        "/labels"
      ]

instance Prelude.ToQuery DeleteLabels where
  toQuery DeleteLabels' {..} =
    Prelude.mconcat
      [ "labels"
          Prelude.=: Prelude.toQuery
            (Prelude.toQueryList "member" Prelude.<$> labels),
        "deleteAll" Prelude.=: deleteAll
      ]

-- | /See:/ 'newDeleteLabelsResponse' smart constructor.
data DeleteLabelsResponse = DeleteLabelsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DeleteLabelsResponse
newDeleteLabelsResponse pHttpStatus_ =
  DeleteLabelsResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteLabelsResponse_httpStatus :: Lens.Lens' DeleteLabelsResponse Prelude.Int
deleteLabelsResponse_httpStatus = Lens.lens (\DeleteLabelsResponse' {httpStatus} -> httpStatus) (\s@DeleteLabelsResponse' {} a -> s {httpStatus = a} :: DeleteLabelsResponse)

instance Prelude.NFData DeleteLabelsResponse
