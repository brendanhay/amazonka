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
-- Module      : Amazonka.ECS.PutAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create or update an attribute on an Amazon ECS resource. If the
-- attribute doesn\'t exist, it\'s created. If the attribute exists, its
-- value is replaced with the specified value. To delete an attribute, use
-- DeleteAttributes. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-placement-constraints.html#attributes Attributes>
-- in the /Amazon Elastic Container Service Developer Guide/.
module Amazonka.ECS.PutAttributes
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutAttributes' smart constructor.
data PutAttributes = PutAttributes'
  { -- | The short name or full Amazon Resource Name (ARN) of the cluster that
    -- contains the resource to apply attributes. If you do not specify a
    -- cluster, the default cluster is assumed.
    cluster :: Prelude.Maybe Prelude.Text,
    -- | The attributes to apply to your resource. You can specify up to 10
    -- custom attributes for each resource. You can specify up to 10 attributes
    -- in a single call.
    attributes :: [Attribute]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- custom attributes for each resource. You can specify up to 10 attributes
-- in a single call.
newPutAttributes ::
  PutAttributes
newPutAttributes =
  PutAttributes'
    { cluster = Prelude.Nothing,
      attributes = Prelude.mempty
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- contains the resource to apply attributes. If you do not specify a
-- cluster, the default cluster is assumed.
putAttributes_cluster :: Lens.Lens' PutAttributes (Prelude.Maybe Prelude.Text)
putAttributes_cluster = Lens.lens (\PutAttributes' {cluster} -> cluster) (\s@PutAttributes' {} a -> s {cluster = a} :: PutAttributes)

-- | The attributes to apply to your resource. You can specify up to 10
-- custom attributes for each resource. You can specify up to 10 attributes
-- in a single call.
putAttributes_attributes :: Lens.Lens' PutAttributes [Attribute]
putAttributes_attributes = Lens.lens (\PutAttributes' {attributes} -> attributes) (\s@PutAttributes' {} a -> s {attributes = a} :: PutAttributes) Prelude.. Lens.coerced

instance Core.AWSRequest PutAttributes where
  type
    AWSResponse PutAttributes =
      PutAttributesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutAttributesResponse'
            Prelude.<$> (x Data..?> "attributes" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutAttributes where
  hashWithSalt _salt PutAttributes' {..} =
    _salt
      `Prelude.hashWithSalt` cluster
      `Prelude.hashWithSalt` attributes

instance Prelude.NFData PutAttributes where
  rnf PutAttributes' {..} =
    Prelude.rnf cluster
      `Prelude.seq` Prelude.rnf attributes

instance Data.ToHeaders PutAttributes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonEC2ContainerServiceV20141113.PutAttributes" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutAttributes where
  toJSON PutAttributes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("cluster" Data..=) Prelude.<$> cluster,
            Prelude.Just ("attributes" Data..= attributes)
          ]
      )

instance Data.ToPath PutAttributes where
  toPath = Prelude.const "/"

instance Data.ToQuery PutAttributes where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutAttributesResponse' smart constructor.
data PutAttributesResponse = PutAttributesResponse'
  { -- | The attributes applied to your resource.
    attributes :: Prelude.Maybe [Attribute],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  PutAttributesResponse
newPutAttributesResponse pHttpStatus_ =
  PutAttributesResponse'
    { attributes =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The attributes applied to your resource.
putAttributesResponse_attributes :: Lens.Lens' PutAttributesResponse (Prelude.Maybe [Attribute])
putAttributesResponse_attributes = Lens.lens (\PutAttributesResponse' {attributes} -> attributes) (\s@PutAttributesResponse' {} a -> s {attributes = a} :: PutAttributesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
putAttributesResponse_httpStatus :: Lens.Lens' PutAttributesResponse Prelude.Int
putAttributesResponse_httpStatus = Lens.lens (\PutAttributesResponse' {httpStatus} -> httpStatus) (\s@PutAttributesResponse' {} a -> s {httpStatus = a} :: PutAttributesResponse)

instance Prelude.NFData PutAttributesResponse where
  rnf PutAttributesResponse' {..} =
    Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf httpStatus
