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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteAttributes' smart constructor.
data DeleteAttributes = DeleteAttributes'
  { -- | The short name or full Amazon Resource Name (ARN) of the cluster that
    -- contains the resource to delete attributes. If you do not specify a
    -- cluster, the default cluster is assumed.
    cluster :: Prelude.Maybe Prelude.Text,
    -- | The attributes to delete from your resource. You can specify up to 10
    -- attributes per request. For custom attributes, specify the attribute
    -- name and target ID, but do not specify the value. If you specify the
    -- target ID using the short form, you must also specify the target type.
    attributes :: [Attribute]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { cluster = Prelude.Nothing,
      attributes = Prelude.mempty
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- contains the resource to delete attributes. If you do not specify a
-- cluster, the default cluster is assumed.
deleteAttributes_cluster :: Lens.Lens' DeleteAttributes (Prelude.Maybe Prelude.Text)
deleteAttributes_cluster = Lens.lens (\DeleteAttributes' {cluster} -> cluster) (\s@DeleteAttributes' {} a -> s {cluster = a} :: DeleteAttributes)

-- | The attributes to delete from your resource. You can specify up to 10
-- attributes per request. For custom attributes, specify the attribute
-- name and target ID, but do not specify the value. If you specify the
-- target ID using the short form, you must also specify the target type.
deleteAttributes_attributes :: Lens.Lens' DeleteAttributes [Attribute]
deleteAttributes_attributes = Lens.lens (\DeleteAttributes' {attributes} -> attributes) (\s@DeleteAttributes' {} a -> s {attributes = a} :: DeleteAttributes) Prelude.. Lens._Coerce

instance Core.AWSRequest DeleteAttributes where
  type
    AWSResponse DeleteAttributes =
      DeleteAttributesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteAttributesResponse'
            Prelude.<$> (x Core..?> "attributes" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteAttributes

instance Prelude.NFData DeleteAttributes

instance Core.ToHeaders DeleteAttributes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerServiceV20141113.DeleteAttributes" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteAttributes where
  toJSON DeleteAttributes' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("cluster" Core..=) Prelude.<$> cluster,
            Prelude.Just ("attributes" Core..= attributes)
          ]
      )

instance Core.ToPath DeleteAttributes where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteAttributes where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAttributesResponse' smart constructor.
data DeleteAttributesResponse = DeleteAttributesResponse'
  { -- | A list of attribute objects that were successfully deleted from your
    -- resource.
    attributes :: Prelude.Maybe [Attribute],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DeleteAttributesResponse
newDeleteAttributesResponse pHttpStatus_ =
  DeleteAttributesResponse'
    { attributes =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of attribute objects that were successfully deleted from your
-- resource.
deleteAttributesResponse_attributes :: Lens.Lens' DeleteAttributesResponse (Prelude.Maybe [Attribute])
deleteAttributesResponse_attributes = Lens.lens (\DeleteAttributesResponse' {attributes} -> attributes) (\s@DeleteAttributesResponse' {} a -> s {attributes = a} :: DeleteAttributesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
deleteAttributesResponse_httpStatus :: Lens.Lens' DeleteAttributesResponse Prelude.Int
deleteAttributesResponse_httpStatus = Lens.lens (\DeleteAttributesResponse' {httpStatus} -> httpStatus) (\s@DeleteAttributesResponse' {} a -> s {httpStatus = a} :: DeleteAttributesResponse)

instance Prelude.NFData DeleteAttributesResponse
