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
-- Module      : Amazonka.FMS.BatchAssociateResource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associate resources to a Firewall Manager resource set.
module Amazonka.FMS.BatchAssociateResource
  ( -- * Creating a Request
    BatchAssociateResource (..),
    newBatchAssociateResource,

    -- * Request Lenses
    batchAssociateResource_resourceSetIdentifier,
    batchAssociateResource_items,

    -- * Destructuring the Response
    BatchAssociateResourceResponse (..),
    newBatchAssociateResourceResponse,

    -- * Response Lenses
    batchAssociateResourceResponse_httpStatus,
    batchAssociateResourceResponse_resourceSetIdentifier,
    batchAssociateResourceResponse_failedItems,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchAssociateResource' smart constructor.
data BatchAssociateResource = BatchAssociateResource'
  { -- | A unique identifier for the resource set, used in a TODO to refer to the
    -- resource set.
    resourceSetIdentifier :: Prelude.Text,
    -- | The uniform resource identifiers (URIs) of resources that should be
    -- associated to the resource set. The URIs must be Amazon Resource Names
    -- (ARNs).
    items :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchAssociateResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceSetIdentifier', 'batchAssociateResource_resourceSetIdentifier' - A unique identifier for the resource set, used in a TODO to refer to the
-- resource set.
--
-- 'items', 'batchAssociateResource_items' - The uniform resource identifiers (URIs) of resources that should be
-- associated to the resource set. The URIs must be Amazon Resource Names
-- (ARNs).
newBatchAssociateResource ::
  -- | 'resourceSetIdentifier'
  Prelude.Text ->
  BatchAssociateResource
newBatchAssociateResource pResourceSetIdentifier_ =
  BatchAssociateResource'
    { resourceSetIdentifier =
        pResourceSetIdentifier_,
      items = Prelude.mempty
    }

-- | A unique identifier for the resource set, used in a TODO to refer to the
-- resource set.
batchAssociateResource_resourceSetIdentifier :: Lens.Lens' BatchAssociateResource Prelude.Text
batchAssociateResource_resourceSetIdentifier = Lens.lens (\BatchAssociateResource' {resourceSetIdentifier} -> resourceSetIdentifier) (\s@BatchAssociateResource' {} a -> s {resourceSetIdentifier = a} :: BatchAssociateResource)

-- | The uniform resource identifiers (URIs) of resources that should be
-- associated to the resource set. The URIs must be Amazon Resource Names
-- (ARNs).
batchAssociateResource_items :: Lens.Lens' BatchAssociateResource [Prelude.Text]
batchAssociateResource_items = Lens.lens (\BatchAssociateResource' {items} -> items) (\s@BatchAssociateResource' {} a -> s {items = a} :: BatchAssociateResource) Prelude.. Lens.coerced

instance Core.AWSRequest BatchAssociateResource where
  type
    AWSResponse BatchAssociateResource =
      BatchAssociateResourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchAssociateResourceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ResourceSetIdentifier")
            Prelude.<*> (x Data..?> "FailedItems" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable BatchAssociateResource where
  hashWithSalt _salt BatchAssociateResource' {..} =
    _salt `Prelude.hashWithSalt` resourceSetIdentifier
      `Prelude.hashWithSalt` items

instance Prelude.NFData BatchAssociateResource where
  rnf BatchAssociateResource' {..} =
    Prelude.rnf resourceSetIdentifier
      `Prelude.seq` Prelude.rnf items

instance Data.ToHeaders BatchAssociateResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSFMS_20180101.BatchAssociateResource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchAssociateResource where
  toJSON BatchAssociateResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ResourceSetIdentifier"
                  Data..= resourceSetIdentifier
              ),
            Prelude.Just ("Items" Data..= items)
          ]
      )

instance Data.ToPath BatchAssociateResource where
  toPath = Prelude.const "/"

instance Data.ToQuery BatchAssociateResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchAssociateResourceResponse' smart constructor.
data BatchAssociateResourceResponse = BatchAssociateResourceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A unique identifier for the resource set, used in a TODO to refer to the
    -- resource set.
    resourceSetIdentifier :: Prelude.Text,
    -- | The resources that failed to associate to the resource set.
    failedItems :: [FailedItem]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchAssociateResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'batchAssociateResourceResponse_httpStatus' - The response's http status code.
--
-- 'resourceSetIdentifier', 'batchAssociateResourceResponse_resourceSetIdentifier' - A unique identifier for the resource set, used in a TODO to refer to the
-- resource set.
--
-- 'failedItems', 'batchAssociateResourceResponse_failedItems' - The resources that failed to associate to the resource set.
newBatchAssociateResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'resourceSetIdentifier'
  Prelude.Text ->
  BatchAssociateResourceResponse
newBatchAssociateResourceResponse
  pHttpStatus_
  pResourceSetIdentifier_ =
    BatchAssociateResourceResponse'
      { httpStatus =
          pHttpStatus_,
        resourceSetIdentifier =
          pResourceSetIdentifier_,
        failedItems = Prelude.mempty
      }

-- | The response's http status code.
batchAssociateResourceResponse_httpStatus :: Lens.Lens' BatchAssociateResourceResponse Prelude.Int
batchAssociateResourceResponse_httpStatus = Lens.lens (\BatchAssociateResourceResponse' {httpStatus} -> httpStatus) (\s@BatchAssociateResourceResponse' {} a -> s {httpStatus = a} :: BatchAssociateResourceResponse)

-- | A unique identifier for the resource set, used in a TODO to refer to the
-- resource set.
batchAssociateResourceResponse_resourceSetIdentifier :: Lens.Lens' BatchAssociateResourceResponse Prelude.Text
batchAssociateResourceResponse_resourceSetIdentifier = Lens.lens (\BatchAssociateResourceResponse' {resourceSetIdentifier} -> resourceSetIdentifier) (\s@BatchAssociateResourceResponse' {} a -> s {resourceSetIdentifier = a} :: BatchAssociateResourceResponse)

-- | The resources that failed to associate to the resource set.
batchAssociateResourceResponse_failedItems :: Lens.Lens' BatchAssociateResourceResponse [FailedItem]
batchAssociateResourceResponse_failedItems = Lens.lens (\BatchAssociateResourceResponse' {failedItems} -> failedItems) (\s@BatchAssociateResourceResponse' {} a -> s {failedItems = a} :: BatchAssociateResourceResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    BatchAssociateResourceResponse
  where
  rnf BatchAssociateResourceResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf resourceSetIdentifier
      `Prelude.seq` Prelude.rnf failedItems
