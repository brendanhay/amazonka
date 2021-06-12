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
-- Module      : Network.AWS.ECS.DeleteAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes one or more custom attributes from an Amazon ECS resource.
module Network.AWS.ECS.DeleteAttributes
  ( -- * Creating a Request
    DeleteAttributes (..),
    newDeleteAttributes,

    -- * Request Lenses
    deleteAttributes_cluster,
    deleteAttributes_attributes,

    -- * Destructuring the Response
    DeleteAttributesResponse (..),
    newDeleteAttributesResponse,

    -- * Response Lenses
    deleteAttributesResponse_attributes,
    deleteAttributesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteAttributes' smart constructor.
data DeleteAttributes = DeleteAttributes'
  { -- | The short name or full Amazon Resource Name (ARN) of the cluster that
    -- contains the resource to delete attributes. If you do not specify a
    -- cluster, the default cluster is assumed.
    cluster :: Core.Maybe Core.Text,
    -- | The attributes to delete from your resource. You can specify up to 10
    -- attributes per request. For custom attributes, specify the attribute
    -- name and target ID, but do not specify the value. If you specify the
    -- target ID using the short form, you must also specify the target type.
    attributes :: [Attribute]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cluster', 'deleteAttributes_cluster' - The short name or full Amazon Resource Name (ARN) of the cluster that
-- contains the resource to delete attributes. If you do not specify a
-- cluster, the default cluster is assumed.
--
-- 'attributes', 'deleteAttributes_attributes' - The attributes to delete from your resource. You can specify up to 10
-- attributes per request. For custom attributes, specify the attribute
-- name and target ID, but do not specify the value. If you specify the
-- target ID using the short form, you must also specify the target type.
newDeleteAttributes ::
  DeleteAttributes
newDeleteAttributes =
  DeleteAttributes'
    { cluster = Core.Nothing,
      attributes = Core.mempty
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- contains the resource to delete attributes. If you do not specify a
-- cluster, the default cluster is assumed.
deleteAttributes_cluster :: Lens.Lens' DeleteAttributes (Core.Maybe Core.Text)
deleteAttributes_cluster = Lens.lens (\DeleteAttributes' {cluster} -> cluster) (\s@DeleteAttributes' {} a -> s {cluster = a} :: DeleteAttributes)

-- | The attributes to delete from your resource. You can specify up to 10
-- attributes per request. For custom attributes, specify the attribute
-- name and target ID, but do not specify the value. If you specify the
-- target ID using the short form, you must also specify the target type.
deleteAttributes_attributes :: Lens.Lens' DeleteAttributes [Attribute]
deleteAttributes_attributes = Lens.lens (\DeleteAttributes' {attributes} -> attributes) (\s@DeleteAttributes' {} a -> s {attributes = a} :: DeleteAttributes) Core.. Lens._Coerce

instance Core.AWSRequest DeleteAttributes where
  type
    AWSResponse DeleteAttributes =
      DeleteAttributesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteAttributesResponse'
            Core.<$> (x Core..?> "attributes" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteAttributes

instance Core.NFData DeleteAttributes

instance Core.ToHeaders DeleteAttributes where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerServiceV20141113.DeleteAttributes" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteAttributes where
  toJSON DeleteAttributes' {..} =
    Core.object
      ( Core.catMaybes
          [ ("cluster" Core..=) Core.<$> cluster,
            Core.Just ("attributes" Core..= attributes)
          ]
      )

instance Core.ToPath DeleteAttributes where
  toPath = Core.const "/"

instance Core.ToQuery DeleteAttributes where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteAttributesResponse' smart constructor.
data DeleteAttributesResponse = DeleteAttributesResponse'
  { -- | A list of attribute objects that were successfully deleted from your
    -- resource.
    attributes :: Core.Maybe [Attribute],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'deleteAttributesResponse_attributes' - A list of attribute objects that were successfully deleted from your
-- resource.
--
-- 'httpStatus', 'deleteAttributesResponse_httpStatus' - The response's http status code.
newDeleteAttributesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteAttributesResponse
newDeleteAttributesResponse pHttpStatus_ =
  DeleteAttributesResponse'
    { attributes =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of attribute objects that were successfully deleted from your
-- resource.
deleteAttributesResponse_attributes :: Lens.Lens' DeleteAttributesResponse (Core.Maybe [Attribute])
deleteAttributesResponse_attributes = Lens.lens (\DeleteAttributesResponse' {attributes} -> attributes) (\s@DeleteAttributesResponse' {} a -> s {attributes = a} :: DeleteAttributesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
deleteAttributesResponse_httpStatus :: Lens.Lens' DeleteAttributesResponse Core.Int
deleteAttributesResponse_httpStatus = Lens.lens (\DeleteAttributesResponse' {httpStatus} -> httpStatus) (\s@DeleteAttributesResponse' {} a -> s {httpStatus = a} :: DeleteAttributesResponse)

instance Core.NFData DeleteAttributesResponse
