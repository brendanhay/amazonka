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
-- Module      : Amazonka.FMS.BatchDisassociateResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates resources from a Firewall Manager resource set.
module Amazonka.FMS.BatchDisassociateResource
  ( -- * Creating a Request
    BatchDisassociateResource (..),
    newBatchDisassociateResource,

    -- * Request Lenses
    batchDisassociateResource_resourceSetIdentifier,
    batchDisassociateResource_items,

    -- * Destructuring the Response
    BatchDisassociateResourceResponse (..),
    newBatchDisassociateResourceResponse,

    -- * Response Lenses
    batchDisassociateResourceResponse_httpStatus,
    batchDisassociateResourceResponse_resourceSetIdentifier,
    batchDisassociateResourceResponse_failedItems,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchDisassociateResource' smart constructor.
data BatchDisassociateResource = BatchDisassociateResource'
  { -- | A unique identifier for the resource set, used in a TODO to refer to the
    -- resource set.
    resourceSetIdentifier :: Prelude.Text,
    -- | The uniform resource identifiers (URI) of resources that should be
    -- disassociated from the resource set. The URIs must be Amazon Resource
    -- Names (ARNs).
    items :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDisassociateResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceSetIdentifier', 'batchDisassociateResource_resourceSetIdentifier' - A unique identifier for the resource set, used in a TODO to refer to the
-- resource set.
--
-- 'items', 'batchDisassociateResource_items' - The uniform resource identifiers (URI) of resources that should be
-- disassociated from the resource set. The URIs must be Amazon Resource
-- Names (ARNs).
newBatchDisassociateResource ::
  -- | 'resourceSetIdentifier'
  Prelude.Text ->
  BatchDisassociateResource
newBatchDisassociateResource pResourceSetIdentifier_ =
  BatchDisassociateResource'
    { resourceSetIdentifier =
        pResourceSetIdentifier_,
      items = Prelude.mempty
    }

-- | A unique identifier for the resource set, used in a TODO to refer to the
-- resource set.
batchDisassociateResource_resourceSetIdentifier :: Lens.Lens' BatchDisassociateResource Prelude.Text
batchDisassociateResource_resourceSetIdentifier = Lens.lens (\BatchDisassociateResource' {resourceSetIdentifier} -> resourceSetIdentifier) (\s@BatchDisassociateResource' {} a -> s {resourceSetIdentifier = a} :: BatchDisassociateResource)

-- | The uniform resource identifiers (URI) of resources that should be
-- disassociated from the resource set. The URIs must be Amazon Resource
-- Names (ARNs).
batchDisassociateResource_items :: Lens.Lens' BatchDisassociateResource [Prelude.Text]
batchDisassociateResource_items = Lens.lens (\BatchDisassociateResource' {items} -> items) (\s@BatchDisassociateResource' {} a -> s {items = a} :: BatchDisassociateResource) Prelude.. Lens.coerced

instance Core.AWSRequest BatchDisassociateResource where
  type
    AWSResponse BatchDisassociateResource =
      BatchDisassociateResourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchDisassociateResourceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ResourceSetIdentifier")
            Prelude.<*> (x Data..?> "FailedItems" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable BatchDisassociateResource where
  hashWithSalt _salt BatchDisassociateResource' {..} =
    _salt
      `Prelude.hashWithSalt` resourceSetIdentifier
      `Prelude.hashWithSalt` items

instance Prelude.NFData BatchDisassociateResource where
  rnf BatchDisassociateResource' {..} =
    Prelude.rnf resourceSetIdentifier
      `Prelude.seq` Prelude.rnf items

instance Data.ToHeaders BatchDisassociateResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSFMS_20180101.BatchDisassociateResource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchDisassociateResource where
  toJSON BatchDisassociateResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ResourceSetIdentifier"
                  Data..= resourceSetIdentifier
              ),
            Prelude.Just ("Items" Data..= items)
          ]
      )

instance Data.ToPath BatchDisassociateResource where
  toPath = Prelude.const "/"

instance Data.ToQuery BatchDisassociateResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchDisassociateResourceResponse' smart constructor.
data BatchDisassociateResourceResponse = BatchDisassociateResourceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A unique identifier for the resource set, used in a TODO to refer to the
    -- resource set.
    resourceSetIdentifier :: Prelude.Text,
    -- | The resources that failed to disassociate from the resource set.
    failedItems :: [FailedItem]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDisassociateResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'batchDisassociateResourceResponse_httpStatus' - The response's http status code.
--
-- 'resourceSetIdentifier', 'batchDisassociateResourceResponse_resourceSetIdentifier' - A unique identifier for the resource set, used in a TODO to refer to the
-- resource set.
--
-- 'failedItems', 'batchDisassociateResourceResponse_failedItems' - The resources that failed to disassociate from the resource set.
newBatchDisassociateResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'resourceSetIdentifier'
  Prelude.Text ->
  BatchDisassociateResourceResponse
newBatchDisassociateResourceResponse
  pHttpStatus_
  pResourceSetIdentifier_ =
    BatchDisassociateResourceResponse'
      { httpStatus =
          pHttpStatus_,
        resourceSetIdentifier =
          pResourceSetIdentifier_,
        failedItems = Prelude.mempty
      }

-- | The response's http status code.
batchDisassociateResourceResponse_httpStatus :: Lens.Lens' BatchDisassociateResourceResponse Prelude.Int
batchDisassociateResourceResponse_httpStatus = Lens.lens (\BatchDisassociateResourceResponse' {httpStatus} -> httpStatus) (\s@BatchDisassociateResourceResponse' {} a -> s {httpStatus = a} :: BatchDisassociateResourceResponse)

-- | A unique identifier for the resource set, used in a TODO to refer to the
-- resource set.
batchDisassociateResourceResponse_resourceSetIdentifier :: Lens.Lens' BatchDisassociateResourceResponse Prelude.Text
batchDisassociateResourceResponse_resourceSetIdentifier = Lens.lens (\BatchDisassociateResourceResponse' {resourceSetIdentifier} -> resourceSetIdentifier) (\s@BatchDisassociateResourceResponse' {} a -> s {resourceSetIdentifier = a} :: BatchDisassociateResourceResponse)

-- | The resources that failed to disassociate from the resource set.
batchDisassociateResourceResponse_failedItems :: Lens.Lens' BatchDisassociateResourceResponse [FailedItem]
batchDisassociateResourceResponse_failedItems = Lens.lens (\BatchDisassociateResourceResponse' {failedItems} -> failedItems) (\s@BatchDisassociateResourceResponse' {} a -> s {failedItems = a} :: BatchDisassociateResourceResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    BatchDisassociateResourceResponse
  where
  rnf BatchDisassociateResourceResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf resourceSetIdentifier
      `Prelude.seq` Prelude.rnf failedItems
