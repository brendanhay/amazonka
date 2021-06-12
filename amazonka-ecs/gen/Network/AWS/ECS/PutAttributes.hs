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
-- Module      : Network.AWS.ECS.PutAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create or update an attribute on an Amazon ECS resource. If the
-- attribute does not exist, it is created. If the attribute exists, its
-- value is replaced with the specified value. To delete an attribute, use
-- DeleteAttributes. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-placement-constraints.html#attributes Attributes>
-- in the /Amazon Elastic Container Service Developer Guide/.
module Network.AWS.ECS.PutAttributes
  ( -- * Creating a Request
    PutAttributes (..),
    newPutAttributes,

    -- * Request Lenses
    putAttributes_cluster,
    putAttributes_attributes,

    -- * Destructuring the Response
    PutAttributesResponse (..),
    newPutAttributesResponse,

    -- * Response Lenses
    putAttributesResponse_attributes,
    putAttributesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutAttributes' smart constructor.
data PutAttributes = PutAttributes'
  { -- | The short name or full Amazon Resource Name (ARN) of the cluster that
    -- contains the resource to apply attributes. If you do not specify a
    -- cluster, the default cluster is assumed.
    cluster :: Core.Maybe Core.Text,
    -- | The attributes to apply to your resource. You can specify up to 10
    -- custom attributes per resource. You can specify up to 10 attributes in a
    -- single call.
    attributes :: [Attribute]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cluster', 'putAttributes_cluster' - The short name or full Amazon Resource Name (ARN) of the cluster that
-- contains the resource to apply attributes. If you do not specify a
-- cluster, the default cluster is assumed.
--
-- 'attributes', 'putAttributes_attributes' - The attributes to apply to your resource. You can specify up to 10
-- custom attributes per resource. You can specify up to 10 attributes in a
-- single call.
newPutAttributes ::
  PutAttributes
newPutAttributes =
  PutAttributes'
    { cluster = Core.Nothing,
      attributes = Core.mempty
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- contains the resource to apply attributes. If you do not specify a
-- cluster, the default cluster is assumed.
putAttributes_cluster :: Lens.Lens' PutAttributes (Core.Maybe Core.Text)
putAttributes_cluster = Lens.lens (\PutAttributes' {cluster} -> cluster) (\s@PutAttributes' {} a -> s {cluster = a} :: PutAttributes)

-- | The attributes to apply to your resource. You can specify up to 10
-- custom attributes per resource. You can specify up to 10 attributes in a
-- single call.
putAttributes_attributes :: Lens.Lens' PutAttributes [Attribute]
putAttributes_attributes = Lens.lens (\PutAttributes' {attributes} -> attributes) (\s@PutAttributes' {} a -> s {attributes = a} :: PutAttributes) Core.. Lens._Coerce

instance Core.AWSRequest PutAttributes where
  type
    AWSResponse PutAttributes =
      PutAttributesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutAttributesResponse'
            Core.<$> (x Core..?> "attributes" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PutAttributes

instance Core.NFData PutAttributes

instance Core.ToHeaders PutAttributes where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerServiceV20141113.PutAttributes" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutAttributes where
  toJSON PutAttributes' {..} =
    Core.object
      ( Core.catMaybes
          [ ("cluster" Core..=) Core.<$> cluster,
            Core.Just ("attributes" Core..= attributes)
          ]
      )

instance Core.ToPath PutAttributes where
  toPath = Core.const "/"

instance Core.ToQuery PutAttributes where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutAttributesResponse' smart constructor.
data PutAttributesResponse = PutAttributesResponse'
  { -- | The attributes applied to your resource.
    attributes :: Core.Maybe [Attribute],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'putAttributesResponse_attributes' - The attributes applied to your resource.
--
-- 'httpStatus', 'putAttributesResponse_httpStatus' - The response's http status code.
newPutAttributesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PutAttributesResponse
newPutAttributesResponse pHttpStatus_ =
  PutAttributesResponse'
    { attributes = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The attributes applied to your resource.
putAttributesResponse_attributes :: Lens.Lens' PutAttributesResponse (Core.Maybe [Attribute])
putAttributesResponse_attributes = Lens.lens (\PutAttributesResponse' {attributes} -> attributes) (\s@PutAttributesResponse' {} a -> s {attributes = a} :: PutAttributesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
putAttributesResponse_httpStatus :: Lens.Lens' PutAttributesResponse Core.Int
putAttributesResponse_httpStatus = Lens.lens (\PutAttributesResponse' {httpStatus} -> httpStatus) (\s@PutAttributesResponse' {} a -> s {httpStatus = a} :: PutAttributesResponse)

instance Core.NFData PutAttributesResponse
